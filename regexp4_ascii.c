#include "regexp4.h"
#include "charUtils.h"

#define TRUE                1
#define FALSE               0
#define NIL                 0
#define INF        1073741824 // 2^30
#define MAX_CATCHS         16
#define MAX_TABLE         256

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
  char           *ptr;
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
} static table[ MAX_TABLE ];

static int table_index;
static int global_mods;

static void tableAppend  ( struct RE *rexp, enum COMMAND command );
static void tableClose   ( int index );

static void path         ( struct RE  rexp );
static void busterPath   ( struct RE *rexp );
static int  isPath       ( struct RE *rexp );
static void bracket      ( struct RE *rexp );
static int  tracker      ( struct RE *rexp, struct RE *track );
static void trackByLen   ( struct RE *rexp, struct RE *track, int len, int type );
static int  trackByType  ( struct RE *rexp, struct RE *track, int type );
static void getMods      ( struct RE *rexp, struct RE *track );
static void getLoops     ( struct RE *rexp, struct RE *track );
static void fwrTrack     ( struct RE *track, int len );
static int  walkMeta     ( char *str );
static int  walkBracket  ( char *str );

void compile( char *re ){
  struct RE    rexp;
  rexp.ptr     = re;
  rexp.type    = PATH;
  rexp.len     = strLen( re );
  rexp.mods    = 0;
  rexp.index   = 0;
  table_index  = 0;

  getMods( &rexp, &rexp );
  global_mods = rexp.mods;

  if( isPath( &rexp ) ) path(  rexp );
  else            busterPath( &rexp );

  tableAppend( NIL, COM_END );
}

static void tableAppend( struct RE *rexp, enum COMMAND command ){
  table[ table_index ].command = command;
  table[ table_index ].close   = table_index;

  if( rexp ) {
    rexp->index = table_index;
    table[ table_index++ ].re = *rexp;
  } else table_index++;
}

static void tableClose( int index ){
  table[ index ].close = table_index;
}

static void path( struct RE rexp ){
  struct RE track;
  tableAppend( &rexp, COM_PATH_INI );

  while( trackByType( &rexp, &track, PATH ) ){
    tableAppend( &track,  COM_PATH_ELE );
    busterPath( &track );
    tableClose( track.index );
  }

  tableClose( rexp.index );
  tableAppend( NIL, COM_PATH_END );
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
      tableAppend(    NIL, COM_HOOK_END  ); break;
    case GROUP  :
      tableAppend( &track, COM_GROUP_INI );
      if( isPath( &track ) ) path( track );
      else                   busterPath( &track );
      tableClose( track.index );
      tableAppend(    NIL, COM_GROUP_END ); break;
    case PATH   :                           break;
    case BRACKET: bracket    ( &track );    break;
    case BACKREF: tableAppend( &track, COM_BACKREF ); break;
    case META   : tableAppend( &track, COM_META    ); break;
    case RANGEAB: tableAppend( &track, COM_RANGEAB ); break;
    case POINT  : tableAppend( &track, COM_POINT   ); break;
    case SIMPLE : tableAppend( &track, COM_SIMPLE  ); break;
    }
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
  if( rexp->len == 0 ) return FALSE;

  switch( *rexp->ptr ) {
  case ':': trackByLen ( rexp, track, 2, META    ); break;
  case '.': trackByLen ( rexp, track, 1, POINT   ); break;
  case '@': trackByLen ( rexp, track, 1 +
                         countCharDigits( rexp->ptr + 1 ),
                                         BACKREF ); break;
  case '(': trackByType( rexp, track,    GROUP   ); break;
  case '<': trackByType( rexp, track,    HOOK    ); break;
  case '[': trackByType( rexp, track,    BRACKET ); break;
  default :
    for( int i = 1; i < rexp->len; i++ )
      switch( rexp->ptr[ i ] ){
      case '(': case '<': case '[': case '@': case ':': case '.':
        trackByLen( rexp, track, i, SIMPLE  ); goto getLM;
      case '?': case '+': case '*': case '{': case '-': case '#':
        if( i == 1 ){
          if( rexp->ptr[ i ] == '-' ) trackByLen( rexp, track, 3, RANGEAB );
          else                        trackByLen( rexp, track, 1, SIMPLE  );
        } else trackByLen( rexp, track, i - 1, SIMPLE  );
        goto getLM;
      }

    trackByLen( rexp, track, rexp->len, SIMPLE  );
  }

 getLM:
  getLoops( rexp, track );
  getMods ( rexp, track );
  return TRUE;
}

static void trackByLen( struct RE *rexp, struct RE *track, int len, int type ){
  *track       = *rexp;
  track->type  = type;
  track->len   = len;
  fwrTrack( rexp, len );
}

static int trackByType( struct RE *rexp, struct RE *track, int type ){
  if( rexp->len == 0 ) return FALSE;

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
  track->mods &= ~MOD_NEGATIVE;

  while( inMods )
    switch( rexp->ptr[ ++pos ] ){
    case '^': track->mods |=  MOD_ALPHA     ; break;
    case '$': track->mods |=  MOD_OMEGA     ; break;
    case '?': track->mods |=  MOD_LONLEY    ; break;
    case '~': track->mods |=  MOD_FwrByChar ; break;
    case '*': track->mods |=  MOD_COMMUNISM ; break;
    case '/': track->mods &= ~MOD_COMMUNISM ; break;
    case '!': track->mods |=  MOD_NEGATIVE  ; break;
    default : inMods       =  FALSE         ; break;
    }

  fwrTrack( rexp, pos );
}

static void getLoops( struct RE *rexp, struct RE *track ){
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

static void bracket( struct RE *rexp ){
  struct RE track;

  if( rexp->ptr[0] == '^' ){
    fwrTrack( rexp, 1 );
    if( rexp->mods & MOD_NEGATIVE ) rexp->mods &= ~MOD_NEGATIVE;
    else                            rexp->mods |=  MOD_NEGATIVE;
  }

  tableAppend( rexp, COM_BRACKET_INI );

  while( tracker( rexp, &track ) ){
    switch( track.type ){
    case META   : tableAppend( &track, COM_META    ); break;
    case RANGEAB: tableAppend( &track, COM_RANGEAB ); break;
    case POINT  : tableAppend( &track, COM_POINT   ); break;
    default     : tableAppend( &track, COM_SIMPLE  ); break;
    }
  }

  tableClose( rexp->index );
  tableAppend( NIL, COM_BRACKET_END );
}

static int  walker       ( int  index );
static int  trekking     ( int  index );
static int  loopGroup    ( int  index );
static int  looper       ( int  index );

static int  match        ( int  index );
static int  matchBracket ( int  index );
static int  matchBackRef ( int  index );
static int  matchRange   ( int  index, int   chr );
static int  matchMeta    ( int  index, int   chr );
static int  matchText    ( int  index, char *txt );

static void openCatch    ( int *index );
static void closeCatch   ( int  index );
static int  lastIdCatch  ( int  id    );

int regexp4( char *txt, char *re ){
  int result   = 0;
  text.len     = strLen( txt );
  Catch.ptr[0] = txt;
  Catch.len[0] = text.len;
  Catch.id [0] = 0;
  Catch.index  = 1;

  if( text.len == 0 || strLen( re ) == 0 ) return 0;

  compile( re );

  for( int oCindex, forward, i = 0, loops = global_mods & MOD_ALPHA ? 1 : text.len; i < loops; i += forward ){
    forward    = 1;
    Catch.idx  = 1;
    oCindex    = Catch.index;
    text.pos   = 0;
    text.ptr   = txt          + i;
    text.len   = Catch.len[0] - i;

    if( trekking( 0 ) ){
      if     (  global_mods & MOD_OMEGA    ){ if( text.pos == text.len ) return TRUE; }
      else if(  global_mods & MOD_LONLEY   )                             return TRUE;
      else if( (global_mods & MOD_FwrByChar) || text.pos == 0 )          result++;
      else   {  forward = text.pos;                                      result++; }
    } else Catch.index = oCindex;
  }

  return result;
}

static int trekking( int index ){
  int iCatch, result = FALSE;

  switch( table[ index ].command ){
  case COM_END        :
  case COM_PATH_END   :
  case COM_PATH_ELE   :
  case COM_GROUP_END  :
  case COM_HOOK_END   :
  case COM_BRACKET_END: return TRUE;
  case COM_PATH_INI   : result = walker   ( index ); break;
  case COM_GROUP_INI  : result = loopGroup( index ); break;
  case COM_HOOK_INI   :
    openCatch( &iCatch );
    if( loopGroup( index ) ){
      closeCatch( iCatch );
      result = TRUE;
    }
    break;
  case COM_BRACKET_INI:
  case COM_BACKREF    :
  case COM_META       :
  case COM_RANGEAB    :
  case COM_POINT      :
  case COM_SIMPLE     : result = looper   ( index ); break;
  }

  if( result && trekking( table[ index ].close + 1 ) ) return TRUE;
  else                                                 return FALSE;
}

static int walker( int index ){
  index++;
  for( const int oCindex = Catch.index, oCidx = Catch.idx, oTpos = text.pos;
       table[ index ].command == COM_PATH_ELE;
       index = table[ index ].close, Catch.index = oCindex, Catch.idx = oCidx, text.pos = oTpos )
    if( table[ index ].re.len && trekking( index + 1 ) ) return TRUE;

  return FALSE;
}

static int loopGroup( int index ){
  int loops = 0, textPos = text.pos;

  if( table[ index ].re.len ){
    if( table[ index ].re.mods & MOD_NEGATIVE ){
      while( loops < table[ index ].re.loopsMax && !trekking( index + 1 ) ){
        textPos++;
        text.pos = textPos;
        loops++;
      }
      text.pos = textPos;
    } else
      while( loops < table[ index ].re.loopsMax && trekking( index + 1 ) )
        loops++;
  }

  return loops < table[ index ].re.loopsMin ? FALSE : TRUE;
}

static int looper( int index ){
  int steps, loops = 0;

  if( table[ index ].re.mods & MOD_NEGATIVE )
    while( loops < table[ index ].re.loopsMax && text.pos < text.len && !match( index ) ){
      text.pos += 1;
      loops++;
    }
  else
    while( loops < table[ index ].re.loopsMax && text.pos < text.len && (steps = match( index )) ){
      text.pos += steps;
      loops++;
    }

  return loops < table[ index ].re.loopsMin ? FALSE : TRUE;
}

static int match( int index ){
  switch( table[index].re.type ){
  case POINT  : return TRUE;
  case BRACKET: return matchBracket( index );
  case BACKREF: return matchBackRef( index );
  case RANGEAB: return matchRange  ( index, text.ptr[ text.pos ] );
  case META   : return matchMeta   ( index, text.ptr[ text.pos ] );
  default     : return matchText   ( index, text.ptr + text.pos  );
  }
}

static int matchText( int index, char *txt ){
  if( table[ index ].re.mods & MOD_COMMUNISM )
    return    strnEqlCommunist( txt, table[ index ].re.ptr, table[ index ].re.len ) ? table[ index ].re.len : 0;
  else return strnEql         ( txt, table[ index ].re.ptr, table[ index ].re.len ) ? table[ index ].re.len : 0;
}

static int matchRange( int index, int chr ){
  if( table[ index ].re.mods & MOD_COMMUNISM ){
    chr = toLower( chr );
    return chr >= toLower( table[ index ].re.ptr[ 0 ] ) && chr <= toLower( table[ index ].re.ptr[ 2 ] );
  } else
    return chr >=          table[ index ].re.ptr[ 0 ]   && chr <=          table[ index ].re.ptr[ 2 ];
}

static int matchMeta( int index, int chr ){
  switch( table[ index ].re.ptr[1] ){
  case 'a' : return  isAlpha( chr );
  case 'A' : return !isAlpha( chr );
  case 'd' : return  isDigit( chr );
  case 'D' : return !isDigit( chr );
  case 'w' : return  isAlnum( chr );
  case 'W' : return !isAlnum( chr );
  case 's' : return  isSpace( chr );
  case 'S' : return !isSpace( chr );
  default  : return table[ index ].re.ptr[1] == chr;
  }
}

static int matchBracket( int index ){
  int result = 0;
  for( index++; result == 0 && table[ index ].command != COM_BRACKET_END; index++ ){
    switch( table[ index ].command ){
    case COM_POINT  :
    case COM_RANGEAB:
    case COM_META   : result = match( index ); break;
    default         :
      if( table[ index ].re.mods & MOD_COMMUNISM )
           result = strnChrCommunist( table[ index ].re.ptr, text.ptr[ text.pos ], table[ index ].re.len  ) != 0;
      else result = strnChr         ( table[ index ].re.ptr, text.ptr[ text.pos ], table[ index ].re.len  ) != 0;
    }

    if( result ) return result;
  }

  return FALSE;
}

static int matchBackRef( int index ){
  int backRefIndex = lastIdCatch( aToi( table[ index ].re.ptr + 1 ) );
  if( gpsCatch( backRefIndex ) == NIL ||
      strnEql( text.ptr + text.pos, gpsCatch( backRefIndex ), lenCatch( backRefIndex ) ) == FALSE )
    return FALSE;
  else return lenCatch( backRefIndex );
}

static int lastIdCatch( int id ){
  for( int index = Catch.index - 1; index > 0; index-- )
    if( Catch.id[ index ] == id ) return index;

  return MAX_CATCHS;
}

static void openCatch( int *index ){
  if( Catch.index < MAX_CATCHS ){
    *index = Catch.index++;
    Catch.ptr[ *index ] = text.ptr + text.pos;
    Catch.id [ *index ] = Catch.idx++;
  } else *index = MAX_CATCHS;
}

static void closeCatch( int index ){
  if( index < MAX_CATCHS )
    Catch.len[ index ] = &text.ptr[ text.pos ] - Catch.ptr[ index ];
}

int totCatch(){ return Catch.index - 1; }

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
