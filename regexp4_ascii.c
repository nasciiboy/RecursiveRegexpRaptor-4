#include "regexp4.h"
#include "charUtils.h"

#define TRUE                1
#define FALSE               0
#define NIL                 0
#define INF        1073741824 // 2^30
#define MAX_CATCHS         16
#define MAX_TABLE        1024

#define MOD_ALPHA           1
#define MOD_OMEGA           2
#define MOD_LONLEY          4
#define MOD_FwrByChar       8
#define MOD_COMMUNISM      16
#define MOD_NEGATIVE      128

struct CATch {
  char *ptr[ MAX_CATCHS ];
  int   len[ MAX_CATCHS ];
  int   id [ MAX_CATCHS ];
  int   idx;
  int   index;
} static Catch;

struct TEXT {
  char *ptr;
  int   pos;
  int   len;
} static text;

enum RE_TYPE { PATH, GROUP, HOOK, BRACKET, BACKREF, META, RANGEAB, POINT, SIMPLE };

struct RE {
  char          *ptr;
  int      len, index;
  enum     RE_TYPE  type;
  unsigned char     mods;
  unsigned int      loopsMin, loopsMax;
};

enum COMMAND { COM_PATH_INI, COM_PATH_ELE, COM_PATH_END, COM_GROUP_INI, COM_GROUP_END,
               COM_HOOK_INI, COM_HOOK_END, COM_BRACKET_INI, COM_BRACKET_END,
               COM_BACKREF, COM_META, COM_RANGEAB, COM_POINT, COM_SIMPLE, COM_END };

struct TABLE {
  enum   COMMAND command;
  struct RE      re;
  int            close;
} table[ MAX_TABLE ];

int table_index;
int global_mods;

static void tableAppend  ( struct RE *rexp,   enum COMMAND command                 );
static void tableClose   (                                               int index );
static void path         ( struct RE  rexp                                         );
static void busterPath   ( struct RE *rexp                                         );
static int  isPath       ( struct RE *rexp                                         );
static int  cutTrack     ( struct RE *rexp  , struct RE *track,          int type  );
static int  tracker      ( struct RE *rexp  , struct RE *track                     );
static int  walkMeta     ( char      *str                                          );
static int  walkBracket  ( char      *str                                          );
static void trackByLen   ( struct RE *rexp  , struct RE *track, int len, int type  );
static char *trackerPoint( char      *points, char      *track, int len            );
static void getMods      ( struct RE *rexp  , struct RE *track                     );
static void setLoops     ( struct RE *rexp  , struct RE *track                     );
static void fwrTrack     ( struct RE *track ,                   int len            );
static void bracket      ( struct RE  rexp                                         );

void compile( char *re ){
  struct RE    rexp;
  rexp.ptr     = re;
  rexp.type    = PATH;
  rexp.len     = strLen( re );
  rexp.mods    = 0;
  rexp.index    = 0;
  table_index  = 0;

  getMods( &rexp, &rexp );
  global_mods = rexp.mods;

  if( isPath( &rexp ) ) path(  rexp );
  else            busterPath( &rexp );

  tableAppend( NIL, COM_END );
}

static void tableAppend( struct RE *rexp, enum COMMAND command ){
  table[ table_index ].command = command;
  table[ table_index ].close   = 0;

  if( rexp ) {
    rexp->index = table_index;
    table[ table_index++ ].re = *rexp;
  } else table_index++;
}

static void tableClose( int index ){
  table[ index ].close = table_index;
}

static void busterPath( struct RE *rexp ){
  struct RE track;
  while( tracker( rexp, &track ) )
    switch( track.type ){
    case HOOK   :
      tableAppend( &track, COM_HOOK_INI  );
      if( isPath( &track ) ) path( track );
      else                   busterPath( &track );
      tableClose( track.index );
      tableAppend(      0, COM_HOOK_END  ); break;
    case GROUP  :
      tableAppend( &track, COM_GROUP_INI );
      if( isPath( &track ) ) path( track );
      else                   busterPath( &track );
      tableClose( track.index );
      tableAppend(      0, COM_GROUP_END ); break;
    case PATH   :                           break;
    case POINT  : tableAppend( &track, COM_POINT ); break;
    case BRACKET: bracket( track ); break;
    case BACKREF:
      track.len = aToi( track.ptr + 1 );
      tableAppend( &track, COM_BACKREF );
      break;
    case RANGEAB: tableAppend( &track, COM_RANGEAB ); break;
    case META   : tableAppend( &track, COM_META    ); break;
    case SIMPLE : tableAppend( &track, COM_SIMPLE  ); break;
    }
}

static void path( struct RE rexp ){
  struct RE track;
  tableAppend( &rexp, COM_PATH_INI );

  while( cutTrack( &rexp, &track, PATH ) ){
    tableAppend( &track,  COM_PATH_ELE );
    busterPath( &track );
    tableClose( track.index );
  }

  tableClose( rexp.index );
  tableAppend( 0, COM_PATH_END );
}

static int cutTrack( struct RE *rexp, struct RE *track, int type ){
  if( !rexp->len ) return FALSE;

  *track      = *rexp;
  track->type = type;
  if( type != PATH ) fwrTrack( track, 1 );

  for( int cut, i = 0, deep = 0; i < rexp->len; i++ ){
    i += walkMeta( rexp->ptr + i );

    switch( rexp->ptr[i] ){
    case '<': case '(': deep++; break;
    case '>': case ')': deep--; break;
    case '[': i += walkBracket( rexp->ptr + i ); break;
    }

    switch( type ){
    case HOOK    : cut = deep == 0; break;
    case GROUP   : cut = deep == 0; break;
    case BRACKET : cut =              rexp->ptr[i] == ']'; break;
    case PATH    : cut = deep == 0 && rexp->ptr[i] == '|'; break;
    }

    if( cut ){
      track->len  = &rexp->ptr[i] - track->ptr;
      fwrTrack( rexp, i + 1 );
      return TRUE;
    }
  }

  fwrTrack( rexp, rexp->len );
  return TRUE;
}

static int isPath( struct RE *rexp ){
  for( int i = 0, deep = 0; i < rexp->len; i++ ){
    i += walkMeta( rexp->ptr + i );

    switch( rexp->ptr[i] ){
    case '<': case '(': deep++; break;
    case '>': case ')': deep--; break;
    case '[': i += walkBracket( rexp->ptr + i ); break;
    case '|': if( deep == 0 ) return TRUE;
    }
  }

  return FALSE;
}

static int tracker( struct RE *rexp, struct RE *track ){
  char *point;

  if( rexp->len ){
    switch( *rexp->ptr ){
    case ':': trackByLen( rexp, track, 2, META    ); break;
    case '.': trackByLen( rexp, track, 1, POINT   ); break;
    case '@': trackByLen( rexp, track, 1 +
                          countCharDigits( rexp->ptr + 1 ),
                                          BACKREF ); break;
    case '(': cutTrack  ( rexp, track,    GROUP   ); break;
    case '<': cutTrack  ( rexp, track,    HOOK    ); break;
    case '[': cutTrack  ( rexp, track,    BRACKET ); break;
    default :
      if( (point = trackerPoint( "(<[@:.?+*{-#", rexp->ptr + 1, rexp->len - 1 )) ){
        switch( *point ){
        case '(': case '<': case '[': case '@': case ':': case '.':
          trackByLen( rexp, track, point - rexp->ptr, SIMPLE  ); break;
        case '?': case '+': case '*': case '{': case '-': case '#':
          if( point - rexp->ptr == 1 ){
            if( *point == '-' ) trackByLen( rexp, track, 3, RANGEAB );
            else                trackByLen( rexp, track, 1, SIMPLE  );
          } else trackByLen( rexp, track, (point - rexp->ptr) - 1, SIMPLE  );
        }
      } else trackByLen( rexp, track, rexp->len, SIMPLE  );
    }

    setLoops( rexp, track );
    getMods ( rexp, track );
    return TRUE;
  }

  return FALSE;
}

static void trackByLen( struct RE *rexp, struct RE *track, int len, int type ){
  *track       = *rexp;
  track->type  = type;
  track->len   = len;
  fwrTrack( rexp, len );
}

static char * trackerPoint( char *points, char *track, int len ){
  for( int pos = 0; pos < len; pos++ )
    if( strChr( points, track[ pos ] ) ) return track + pos;

  return 0;
}

static int walkBracket( char *str ){
  int i = 0;
  while( TRUE )
    switch( str[ i ] ){
    case ']': return i;
    case ':': i += 2; break;
    default : i++   ; break;
    }
}

static int walkMeta( char *str ){
  for( int i = 0; ; i += 2 )
    if( str[i] != ':' ) return i;
}

static void fwrTrack( struct RE *track, int len ){
  track->ptr += len; track->len -= len;
}

static void getMods( struct RE *rexp, struct RE *track ){
  int inMods = *rexp->ptr == '#', pos = 0;

  while( inMods )
    switch( rexp->ptr[ ++pos ] ){
    case '^': track->mods |=  MOD_ALPHA     ; break;
    case '$': track->mods |=  MOD_OMEGA     ; break;
    case '?': track->mods |=  MOD_LONLEY    ; break;
    case '~': track->mods |=  MOD_FwrByChar ; break;
    case '*': track->mods |=  MOD_COMMUNISM ; break;
    case '/': track->mods &= ~MOD_COMMUNISM ; break;
    default : inMods       =  FALSE         ; break;
    }

  fwrTrack( rexp, pos );
}

static void setLoops( struct RE *rexp, struct RE *track ){
  track->loopsMin = 1; track->loopsMax = 1;
  int len = 0;

  if( rexp->len )
    switch( *rexp->ptr ){
    case '?' : len = 1; track->loopsMin = 0; track->loopsMax =   1; break;
    case '+' : len = 1; track->loopsMin = 1; track->loopsMax = INF; break;
    case '*' : len = 1; track->loopsMin = 0; track->loopsMax = INF; break;
    case '{' :
      track->loopsMin = aToi( rexp->ptr + 1 ) ;
      if( rexp->ptr[ 1 + countCharDigits( rexp->ptr + 1 ) ] == ',' )
        track->loopsMax = aToi( strChr( rexp->ptr, ',' ) + 1 );
      else
        track->loopsMax = track->loopsMin;

      len = strChr( rexp->ptr, '}' ) - rexp->ptr + 1;
    }

  fwrTrack( rexp, len );
}

static void bracket( struct RE rexp ){
  struct RE track;

  if( *rexp.ptr == '^' ){
    fwrTrack( &rexp, 1 );
    rexp.mods |=  MOD_NEGATIVE;
  }

  tableAppend( &rexp, COM_BRACKET_INI );

  while( tracker( &rexp, &track ) ){
    switch( track.type ){
    case GROUP  :
      tableAppend( &track, COM_GROUP_INI  );
      if( isPath( &track ) ) path( track );
      else                   busterPath( &track );
      tableClose( track.index );
      tableAppend( 0, COM_GROUP_END ); break;
    case POINT  : tableAppend( &track, COM_POINT   ); break;
    case RANGEAB: tableAppend( &track, COM_RANGEAB ); break;
    case META   : tableAppend( &track, COM_META    ); break;
    default     : tableAppend( &track, COM_SIMPLE  ); break;
    }
  }

  tableClose( rexp.index );
  tableAppend( 0, COM_BRACKET_END );
}

static int  walker       ( int *index );
static int  loopGroup    ( int *index );
static int  loopBracket  ( int *index );
static int  trekking     ( int index );
static int  looper       ( int index );

static int  match        ( int index );
static int  matchBracket ( int index );
static int  matchBackRef ( int index );
static int  matchRange   ( int index, char  chr );
static int  matchMeta    ( int index, int   c   );
static int  matchText    ( int index, char *txt );

static void openCatch    ( int  *index );
static void closeCatch   ( int   index );
static int  lastIdCatch  ( int   id    );

void showindex( int index );

int regexp4( char *txt, char *re ){
  int result   = 0;
  text.len     = strLen( txt );
  Catch.ptr[0] = txt;
  Catch.len[0] = text.len;
  Catch.id [0] = 0;
  Catch.index  = 1;

  if( text.len == 0 || strLen( re ) == 0 ) return 0;

  compile( re );

  for( int forward, i = 0, loops = global_mods & MOD_ALPHA ? 1 : text.len; i < loops; i += forward ){
    forward    = 1;
    Catch.idx  = 1;
    text.pos   = 0;
    text.ptr   = txt   + i;
    text.len   = Catch.len[0] - i;

    if( trekking( 0 ) ){
      if     (  global_mods & MOD_OMEGA    ){ if( text.pos == text.len ) return TRUE; }
      else if(  global_mods & MOD_LONLEY   )                             return TRUE;
      else if( (global_mods & MOD_FwrByChar) || text.pos == 0 )          result++;
      else   {  forward = text.pos;                                      result++; }
    }
  }

  return result;
}

static int trekking( int index ){
  int iCatch = MAX_CATCHS, oCindex = Catch.index, oCidx = Catch.idx, oTpos = text.pos;
  int result = FALSE;

  switch( table[ index ].command ){
  case COM_END        : return TRUE;
  case COM_PATH_INI   : result = walker( &index ); break;
  case COM_PATH_END   : return TRUE;
  case COM_PATH_ELE   : return TRUE;
  case COM_GROUP_INI  : result = loopGroup( &index ); break;
  case COM_GROUP_END  : return TRUE;
  case COM_HOOK_INI   :
    openCatch( &iCatch );
    if( loopGroup( &index ) ){
      closeCatch( iCatch );
      result = TRUE;
    } break;
  case COM_HOOK_END   : return TRUE;
  case COM_BRACKET_INI: result = loopBracket( &index ); break;
  case COM_BRACKET_END: return TRUE;
  case COM_BACKREF    :
  case COM_META       :
  case COM_RANGEAB    :
  case COM_POINT      :
  case COM_SIMPLE     : result = looper( index ); break;
  }

  if( result && trekking( index + 1 ) )
    return TRUE;
  else {
    text.pos    = oTpos;
    Catch.index = oCindex;
    Catch.idx   = oCidx;
    return        FALSE;
  }
}

static int walker( int *index ){
  for( int next = *index + 1; table[ next ].command == COM_PATH_ELE; next = table[ next ].close )
    if( table[ next ].re.len && trekking( next + 1 ) ){
      *index = table[ *index ].close;
      return TRUE;
    }

  return FALSE;
}

static int loopGroup( int *index ){
  int loops = 0;

  if( table[ *index ].re.len )
    while( loops < table[ *index ].re.loopsMax && trekking( *index + 1 ) )
      loops++;

  if( loops >= table[ *index ].re.loopsMin ){
    *index = table[ *index ].close;
    return TRUE;
  } else return FALSE;
}

static int loopBracket( int *index ){
  int steps, loops = 0;

  while( loops < table[ *index ].re.loopsMax && text.pos < text.len && (steps = matchBracket( *index )) ){
    text.pos += steps;
    loops++;
  }

  if( loops >= table[ *index ].re.loopsMin ){
    *index = table[ *index ].close;
    return TRUE;
  } else return FALSE;
}


static int looper( int index ){
  int steps, loops = 0;

  while( loops < table[ index ].re.loopsMax && text.pos < text.len && (steps = match( index )) ){
    text.pos += steps;
    loops++;
  }

  return loops < table[ index ].re.loopsMin ? FALSE : TRUE;
}

static int match( int index ){
  switch( table[index].re.type ){
  case POINT  : return TRUE;
  case BACKREF: return matchBackRef( index );
  case RANGEAB: return matchRange  ( index, text.ptr[ text.pos ] );
  case META   : return matchMeta   ( index, text.ptr[ text.pos ] );
  default     : return matchText   ( index, text.ptr + text.pos  );
  }
}

static int matchText( int index, char *txt ){
  if( table[ index ].re.mods & MOD_COMMUNISM )
    return    strnCmpCommunist( txt, table[ index ].re.ptr, table[ index ].re.len )  == 0 ? table[ index ].re.len : 0;
  else return strnCmp         ( txt, table[ index ].re.ptr, table[ index ].re.len )  == 0 ? table[ index ].re.len : 0;
}

static int matchRange( int index, char chr ){
  if( table[ index ].re.mods & MOD_COMMUNISM ){
    chr = toLower( chr );
    return chr >= toLower( table[ index ].re.ptr[ 0 ] ) && chr <= toLower( table[ index ].re.ptr[ 2 ] );
  }

  return chr >= table[ index ].re.ptr[ 0 ] && chr <= table[ index ].re.ptr[ 2 ];
}

static int matchMeta( int index, int c ){
  switch( table[ index ].re.ptr[1] ){
  case 'a' : return  isAlpha( c );
  case 'A' : return !isAlpha( c );
  case 'd' : return  isDigit( c );
  case 'D' : return !isDigit( c );
  case 'w' : return  isAlnum( c );
  case 'W' : return !isAlnum( c );
  case 's' : return  isSpace( c );
  case 'S' : return !isSpace( c );
  default  : return c == table[ index ].re.ptr[1];
  }
}

static int matchBracket( int index ){
  int result = 0, oTpos = text.pos;
  for( int next = index + 1; result == 0 && table[ next ].command != COM_BRACKET_END; next++ ){
    switch( table[ next ].command ){
    case COM_GROUP_INI  :
      if( loopGroup( &next ) ){
        text.pos = oTpos;
        return FALSE;
      }  text.pos = oTpos; return TRUE;
    case COM_POINT      :
    case COM_RANGEAB    :
    case COM_META       : result = match( next ); break;
    default             :
      if( table[ next ].re.mods & MOD_COMMUNISM )
           result = strnChrCommunist( table[ next ].re.ptr, text.ptr[ text.pos ], table[ next ].re.len  ) != 0;
      else result = strnChr         ( table[ next ].re.ptr, text.ptr[ text.pos ], table[ next ].re.len  ) != 0;
    }

    if( result ) return table[ index ].re.mods & MOD_NEGATIVE ? FALSE : result;
  }

  return table[ index ].re.mods & MOD_NEGATIVE ? TRUE : FALSE;
}

static int matchBackRef( int index ){
  int backRefIndex = lastIdCatch( table[ index ].re.len );
  if( gpsCatch( backRefIndex ) == 0 ||
      strnCmp( text.ptr + text.pos, gpsCatch( backRefIndex ), lenCatch( backRefIndex ) ) != 0 )
    return FALSE;
  else return lenCatch( backRefIndex );

  return TRUE;
}

static void openCatch( int *index ){
  if( Catch.index < MAX_CATCHS ){
    *index = Catch.index++;
    Catch.ptr[ *index ] = text.ptr + text.pos;
    Catch.id [ *index ] = Catch.idx++;
  }
}

static void closeCatch( int index ){
  if( index < MAX_CATCHS )
    Catch.len[ index ] = &text.ptr[ text.pos ] - Catch.ptr[ index ];
}

static int lastIdCatch( int id ){
  int lastId = MAX_CATCHS;
  for( int index = 1; index < Catch.index; index++ )
    if( Catch.id[ index ] == id ) lastId = index;

  return lastId;
}

int totalCatch(){ return Catch.index - 1; }

char * gpsCatch( int index ){
  return ( index > 0 && index < Catch.index ) ? Catch.ptr[ index ] : 0;
}

int lenCatch( int index ){
  return ( index > 0 && index < Catch.index ) ? Catch.len[ index ] : 0;
}

char * cpyCatch( char * str, int index ){
  if( index > 0 && index < Catch.index )
    strnCpy( str, Catch.ptr[ index ], Catch.len[ index ] );
  else *str = '\0';

  return str;
}

char * rplCatch( char * newStr, char * rplStr, int id ){
  char *oNewStr = newStr, *text = Catch.ptr[ 0 ];
  strCpy( newStr, text );

  for( int index = 1; index < Catch.index; index++ )
    if( Catch.id[ index ] == id ){
      newStr += Catch.ptr[ index ] - text;
      strCpy( newStr, rplStr );
      newStr += strLen( rplStr );
      text    = Catch.ptr[ index ] + Catch.len[ index ];
      strCpy( newStr, text );
    }

  return oNewStr;
}

char * putCatch( char * newStr, char * putStr ){
  int  index; char *pos, *oNewStr = newStr;
  strCpy( newStr, putStr );

  while( (pos = strChr( putStr, '#' )) ){
    if( pos[ 1 ] == '#' ){
      newStr += pos + 1 - putStr;
      putStr  = pos + 2;
    } else {
      index   = aToi( pos + 1 );
      newStr += pos - putStr;
      cpyCatch( newStr, index );
      newStr += lenCatch( index );
      putStr  = pos + 1 + countCharDigits( pos + 1 );
    }

    strCpy( newStr, putStr );
  }

  return oNewStr;
}
