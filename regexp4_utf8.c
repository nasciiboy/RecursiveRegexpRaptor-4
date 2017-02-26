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
  const char *ptr[ MAX_CATCHS ];
  int   len[ MAX_CATCHS ];
  int   id [ MAX_CATCHS ];
  int   idx;
  int   index;
} static Catch;

struct TEXT {
  const char *ptr;
  int   pos;
  int   len;
} static text;

enum RE_TYPE { PATH, GROUP, HOOK, SET, BACKREF, META, RANGEAB, UTF8, POINT, SIMPLE };

struct RE {
  const    char    *ptr;
  unsigned int      len, index;
  enum     RE_TYPE  type;
  unsigned char     mods;
  unsigned int      loopsMin, loopsMax;
};

enum COMMAND { COM_PATH_INI, COM_PATH_ELE, COM_PATH_END, COM_GROUP_INI, COM_GROUP_END,
               COM_HOOK_INI, COM_HOOK_END, COM_SET_INI, COM_SET_END,
               COM_BACKREF, COM_META, COM_RANGEAB, COM_UTF8, COM_POINT, COM_SIMPLE, COM_END };

struct TABLE {
  enum   COMMAND command;
  struct RE      re;
  int            close;
} static table[ MAX_TABLE ];

int table_index;
int global_mods;

static void tableAppend ( struct RE *rexp, enum COMMAND command );
static void tableClose  ( const int index );

static void genPaths    ( struct RE  rexp );
static void genTracks   ( struct RE *rexp );
static void genSet      ( struct RE *rexp );
static int  isPath      ( struct RE *rexp );
static int  tracker     ( struct RE *rexp, struct RE *track );
static int  trackerSet  ( struct RE *rexp, struct RE *track );

static void cutSimple   ( struct RE *rexp, struct RE *track );
static int  cutByType   ( struct RE *rexp, struct RE *track, const int type );
static void cutByLen    ( struct RE *rexp, struct RE *track, const int len, const int type );
static void cutRexp     ( struct RE *rexp, const  int len );

static void getMods     ( struct RE *rexp, struct RE *track );
static void getLoops    ( struct RE *rexp, struct RE *track );

static int  walkSet     ( const char *str, const  int len );
static int  walkMeta    ( const char *str, const  int len );

static const unsigned char xooooooo = 0x80; // instead `isUTF8( c )` use `c & xooooooo`

static int utf8meter( const char *str ){
  static unsigned char xxoooooo = 0xC0;
  unsigned char i, utfOrNo = *str;

  if( utfOrNo & xooooooo ){
    for ( i = 1, utfOrNo <<= 1; utfOrNo & xooooooo; i++, utfOrNo <<= 1 )
      if( (str[ i ] & xxoooooo) != xooooooo ) return 1;

    if( i >= 2 && i <= 8 ) return i;
  }

  return *str ? 1 : 0;
}

void compile( const char *re ){
  struct RE    rexp;
  rexp.ptr     = re;
  rexp.type    = PATH;
  rexp.len     = strLen( re );
  rexp.mods    = 0;
  rexp.index   = 0;
  table_index  = 0;

  getMods( &rexp, &rexp );
  global_mods = rexp.mods;

  if( isPath( &rexp ) ) genPaths (  rexp );
  else                  genTracks( &rexp );

  tableAppend( NIL, COM_END );
}

static void tableAppend( struct RE *rexp, enum COMMAND command ){
  table[ table_index ].command = command;
  table[ table_index ].close   = table_index;

  if( rexp ) {
    rexp->index = table_index;
    table[ table_index ].re = *rexp;
  }

  table_index++;
}

static void tableClose( const int index ){
  table[ index ].close = table_index;
}

static void genPaths( struct RE rexp ){
  struct RE track;
  tableAppend( &rexp, COM_PATH_INI );

  while( cutByType( &rexp, &track, PATH ) ){
    tableAppend( &track,  COM_PATH_ELE );
    genTracks( &track );
    tableClose( track.index );
  }

  tableClose( rexp.index );
  tableAppend( NIL, COM_PATH_END );
}

static void genTracks( struct RE *rexp ){
  struct RE track;
  while( tracker( rexp, &track ) )
    switch( track.type ){
    case HOOK   :
      tableAppend( &track, COM_HOOK_INI  );
      if( isPath( &track ) ) genPaths (  track );
      else                   genTracks( &track );
      tableClose( track.index );
      tableAppend(    NIL, COM_HOOK_END  ); break;
    case GROUP  :
      tableAppend( &track, COM_GROUP_INI );
      if( isPath( &track ) ) genPaths (  track );
      else                   genTracks( &track );
      tableClose( track.index );
      tableAppend(    NIL, COM_GROUP_END ); break;
    case PATH   :                           break;
    case SET    : genSet     ( &track );    break;
    case BACKREF: tableAppend( &track, COM_BACKREF ); break;
    case META   : tableAppend( &track, COM_META    ); break;
    case UTF8   : tableAppend( &track, COM_UTF8    ); break;
    case POINT  : tableAppend( &track, COM_POINT   ); break;
    default     : tableAppend( &track, COM_SIMPLE  ); break;
    }
}

static int isPath( struct RE *rexp ){
  for( int i = 0, deep = 0; (i += walkMeta( rexp->ptr + i, rexp->len - i )) < rexp->len; i++ )
    switch( rexp->ptr[i] ){
    case '(': case '<': deep++; break;
    case ')': case '>': deep--; break;
    case '[': i += walkSet( rexp->ptr + i, rexp->len - i ); break;
    case '|': if( deep == 0 ) return TRUE;
    }

  return FALSE;
}

static int tracker( struct RE *rexp, struct RE *track ){
  if( rexp->len == 0 ) return FALSE;

  switch( *rexp->ptr & xooooooo ? UTF8 : *rexp->ptr ){
  case ':' : cutByLen ( rexp, track, 2,                    META    ); break;
  case '.' : cutByLen ( rexp, track, 1,                    POINT   ); break;
  case '@' : cutByLen ( rexp, track, 1 +
                                    countCharDigits( rexp->ptr + 1 ),
                                                           BACKREF ); break;
  case '(' : cutByType( rexp, track,                       GROUP   ); break;
  case '<' : cutByType( rexp, track,                       HOOK    ); break;
  case '[' : cutByType( rexp, track,                       SET     ); break;
  case UTF8: cutByLen ( rexp, track, utf8meter(rexp->ptr), UTF8    ); break;
  default  : cutSimple( rexp, track                                ); break;
  }

  getLoops( rexp, track );
  getMods ( rexp, track );
  return TRUE;
}

static void cutSimple( struct RE *rexp, struct RE *track ){
  for( int i = 1; i < rexp->len; i++ )
    switch( rexp->ptr[ i ] & xooooooo ? UTF8 : rexp->ptr[ i ] ){
    case '(': case '<': case '[': case '@': case ':': case '.': case UTF8:
      cutByLen( rexp, track, i, SIMPLE  ); return;
    case '?': case '+': case '*': case '{': case '#':
      if( i == 1 ) cutByLen( rexp, track,     1, SIMPLE );
      else         cutByLen( rexp, track, i - 1, SIMPLE );
      return;
    }

  cutByLen( rexp, track, rexp->len, SIMPLE  );
}

static void cutByLen( struct RE *rexp, struct RE *track, const int len, const int type ){
  *track       = *rexp;
  track->type  = type;
  track->len   = len;
  cutRexp( rexp, len );
}

static int cutByType( struct RE *rexp, struct RE *track, const int type ){
  if( rexp->len == 0 ) return FALSE;

  *track = *rexp;
  track->type = type;
  for( int cut, i = 0, deep = 0; (i += walkMeta( rexp->ptr + i, rexp->len - i )) < rexp->len; i++ ){
    switch( rexp->ptr[ i ] ){
    case '(': case '<': deep++; break;
    case ')': case '>': deep--; break;
    case '[': i += walkSet( rexp->ptr + i, rexp->len - i ); break;
    }

    switch( type ){
    case HOOK    : cut = deep == 0; break;
    case GROUP   : cut = deep == 0; break;
    case SET     : cut = rexp->ptr[i] == ']'; break;
    case PATH    : cut = deep == 0 && rexp->ptr[i] == '|'; break;
    }

    if( cut ){
      track->len  = i;
      cutRexp( rexp, i + 1 );
      if( type != PATH ) cutRexp( track, 1 );
      return TRUE;
    }
  }

  cutRexp( rexp, rexp->len );
  return TRUE;
}

static void cutRexp( struct RE *rexp, const int len ){
  rexp->ptr += len; rexp->len -= len;
}

static int walkSet( const char *str, const int len ){
  for( int i = 0; (i += walkMeta( str + i, len - i )) < len; i++ )
    if( str[i] == ']' ) return i;

  return len;
}

static int walkMeta( const char *str, const int len ){
  for( int i = 0; i < len; i += 2 )
    if( str[i] != ':' ) return i;

  return len;
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

  cutRexp( rexp, pos );
}

static void getLoops( struct RE *rexp, struct RE *track ){
  track->loopsMin = 1; track->loopsMax = 1;

  if( rexp->len )
    switch( *rexp->ptr ){
    case '?' : cutRexp( rexp, 1 ); track->loopsMin = 0; track->loopsMax =   1; return;
    case '+' : cutRexp( rexp, 1 ); track->loopsMin = 1; track->loopsMax = INF; return;
    case '*' : cutRexp( rexp, 1 ); track->loopsMin = 0; track->loopsMax = INF; return;
    case '{' : cutRexp( rexp, 1 );
      track->loopsMin = aToi( rexp->ptr );
      cutRexp( rexp, countCharDigits( rexp->ptr ) );
      if( *rexp->ptr == ',' ){
        cutRexp( rexp, 1 );
        if( *rexp->ptr == '}' )
          track->loopsMax = INF;
        else {
          track->loopsMax = aToi( rexp->ptr );
          cutRexp( rexp, countCharDigits( rexp->ptr  ) );
        }
      } else track->loopsMax = track->loopsMin;

      cutRexp( rexp, 1 );
    }
}

static void genSet( struct RE *rexp ){
  struct RE track;

  if( rexp->ptr[0] == '^' ){
    cutRexp( rexp, 1 );
    if( rexp->mods & MOD_NEGATIVE ) rexp->mods &= ~MOD_NEGATIVE;
    else                            rexp->mods |=  MOD_NEGATIVE;
  }

  tableAppend( rexp, COM_SET_INI );

  while( trackerSet( rexp, &track ) ){
    switch( track.type ){
    case META   : tableAppend( &track, COM_META    ); break;
    case RANGEAB: tableAppend( &track, COM_RANGEAB ); break;
    case UTF8   : tableAppend( &track, COM_UTF8    ); break;
    default     : tableAppend( &track, COM_SIMPLE  ); break;
    }
  }

  tableClose( rexp->index );
  tableAppend( NIL, COM_SET_END );
}

static int trackerSet( struct RE *rexp, struct RE *track ){
  if( rexp->len == 0 ) return FALSE;

  switch( *rexp->ptr & xooooooo ? UTF8 : *rexp->ptr ){
  case ':' : cutByLen ( rexp, track, 2,                    META    ); break;
  case UTF8: cutByLen ( rexp, track, utf8meter(rexp->ptr), UTF8    ); break;
  default  :
    for( int i = 1; i < rexp->len; i++ )
      switch( rexp->ptr[ i ] & xooooooo ? UTF8 : rexp->ptr[ i ] ){
      case ':': case UTF8:
        cutByLen( rexp, track, i, SIMPLE  ); goto setL;
      case '-':
        if( i == 1 ) cutByLen( rexp, track,     3, RANGEAB );
        else         cutByLen( rexp, track, i - 1, SIMPLE  );
        goto setL;
      }

    cutByLen( rexp, track, rexp->len, SIMPLE  );
  }

 setL:
  track->loopsMin = track->loopsMax = 1;
  return TRUE;
}

static int  walker       ( const int  index );
static int  trekking     ( int  index );
static int  loopGroup    ( const int index );
static int  looper       ( const int index );

static int  match        ( const int  index );
static int  matchSet     ( int  index );
static int  matchBackRef ( const int  index );
static int  matchRange   ( const int  index, int   chr );
static int  matchMeta    ( const int  index, const char *txt );
static int  matchText    ( const int  index, const char *txt );

static void openCatch    ( int *index );
static void closeCatch   ( const int  index );
static int  lastIdCatch  ( const int  id    );

int regexp4( const char *txt, const char *re ){
  int result   = 0;
  text.len     = strLen( txt );
  Catch.ptr[0] = txt;
  Catch.len[0] = text.len;
  Catch.id [0] = 0;
  Catch.index  = 1;

  if( text.len == 0 || strLen( re ) == 0 ) return 0;

  compile( re );

  for( int oCindex, forward, i = 0, loops = global_mods & MOD_ALPHA ? 1 : text.len; i < loops; i += forward ){
    forward    = utf8meter( txt + i );
    Catch.idx  = 1;
    oCindex    = Catch.index;
    text.pos   = 0;
    text.ptr   = txt          + i;
    text.len   = Catch.len[0] - i;

    if( trekking( 0 ) ){
      if     (  global_mods & MOD_OMEGA    ){
        if( text.pos == text.len ) return TRUE;
        else Catch.index = 1;
      }
      else if(  global_mods & MOD_LONLEY   )                             return TRUE;
      else if( (global_mods & MOD_FwrByChar) || text.pos == 0 )          result++;
      else   {  forward = text.pos;                                      result++; }
    } else Catch.index = oCindex;
  }

  return result;
}

static int trekking( const int index ){
  int iCatch, result = FALSE;

  switch( table[ index ].command ){
  case COM_END        :
  case COM_PATH_END   :
  case COM_PATH_ELE   :
  case COM_GROUP_END  :
  case COM_HOOK_END   :
  case COM_SET_END    : return TRUE;
  case COM_PATH_INI   : result = walker   ( index ); break;
  case COM_GROUP_INI  : result = loopGroup( index ); break;
  case COM_HOOK_INI   :
    openCatch( &iCatch );
    if( loopGroup( index ) ){
      closeCatch( iCatch );
      result = TRUE;
    }
    break;
  // COM_SET_INI COM_BACKREF COM_META COM_UTF8 COM_POINT !COM_RANGEAB COM_SIMPLE
  default             : result = looper   ( index ); break;
  }

  if( result && trekking( table[ index ].close + 1 ) ) return TRUE;

  return FALSE;
}

static int walker( int index ){
  index++;
  for( const int oCindex = Catch.index, oCidx = Catch.idx, oTpos = text.pos;
       table[ index ].command == COM_PATH_ELE;
       index = table[ index ].close, Catch.index = oCindex, Catch.idx = oCidx, text.pos = oTpos )
    if( trekking( index + 1 ) ) return TRUE;

  return FALSE;
}

static int loopGroup( const int index ){
  int loops = 0, textPos = text.pos;

  if( table[ index ].re.mods & MOD_NEGATIVE ){
    while( loops < table[ index ].re.loopsMax && !trekking( index + 1 ) ){
      textPos  += utf8meter( text.ptr + textPos );
      text.pos  = textPos;
      loops++;
    }
    text.pos = textPos;
  } else
    while( loops < table[ index ].re.loopsMax && trekking( index + 1 ) )
      loops++;

  return loops < table[ index ].re.loopsMin ? FALSE : TRUE;
}

static int looper( const int index ){
  int steps, loops = 0;

  if( table[ index ].re.mods & MOD_NEGATIVE )
    while( loops < table[ index ].re.loopsMax && text.pos < text.len && !match( index ) ){
      text.pos += utf8meter( text.ptr + text.pos );
      loops++;
    }
  else
    while( loops < table[ index ].re.loopsMax && text.pos < text.len && (steps = match( index )) ){
      text.pos += steps;
      loops++;
    }

  return loops < table[ index ].re.loopsMin ? FALSE : TRUE;
}

static int match( const int index ){
  switch( table[index].re.type ){
  case POINT  : return utf8meter( text.ptr + text.pos );
  case SET    : return matchSet    ( index );
  case BACKREF: return matchBackRef( index );
  case META   : return matchMeta   ( index, text.ptr + text.pos );
  default     : return matchText   ( index, text.ptr + text.pos );
  }
}

static int matchText( const int index, const char *txt ){
  if( table[ index ].re.mods & MOD_COMMUNISM )
    return    strnEqlCommunist( txt, table[ index ].re.ptr, table[ index ].re.len ) ? table[ index ].re.len : 0;
  else return strnEql         ( txt, table[ index ].re.ptr, table[ index ].re.len ) ? table[ index ].re.len : 0;
}

static int matchMeta( const int index, const char *txt ){
  switch( table[ index ].re.ptr[1] ){
  case 'a' : return  isAlpha( *txt );
  case 'A' : return !isAlpha( *txt ) ? utf8meter( txt ) : FALSE;
  case 'd' : return  isDigit( *txt );
  case 'D' : return !isDigit( *txt ) ? utf8meter( txt ) : FALSE;
  case 'w' : return  isAlnum( *txt );
  case 'W' : return !isAlnum( *txt ) ? utf8meter( txt ) : FALSE;
  case 's' : return  isSpace( *txt );
  case 'S' : return !isSpace( *txt ) ? utf8meter( txt ) : FALSE;
  case '&' : return *txt & xooooooo  ? utf8meter( txt ) : FALSE;
  default  : return *txt == table[ index ].re.ptr[1];
  }
}

static int matchSet( int index ){
  int result = 0;
  for( index++; result == 0 && table[ index ].command != COM_SET_END; index++ ){
    switch( table[ index ].command ){
    case COM_RANGEAB: result = matchRange( index, text.ptr[ text.pos ] ); break;
    case COM_UTF8   :
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

static int matchRange( const int index, int chr ){
  if( table[ index ].re.mods & MOD_COMMUNISM ){
    chr = toLower( chr );
    return chr >= toLower( table[ index ].re.ptr[ 0 ] ) && chr <= toLower( table[ index ].re.ptr[ 2 ] );
  } else
    return chr >=          table[ index ].re.ptr[ 0 ]   && chr <=          table[ index ].re.ptr[ 2 ];
}

static int matchBackRef( const int index ){
  const int backRefId    = aToi( table[ index ].re.ptr + 1 );
  const int backRefIndex = lastIdCatch( backRefId );
  if( gpsCatch( backRefIndex ) == NIL ||
      strnEql( text.ptr + text.pos, gpsCatch( backRefIndex ), lenCatch( backRefIndex ) ) == FALSE )
    return FALSE;
  else return lenCatch( backRefIndex );
}

static int lastIdCatch( const int id ){
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

static void closeCatch( const int index ){
  if( index < MAX_CATCHS )
    Catch.len[ index ] = &text.ptr[ text.pos ] - Catch.ptr[ index ];
}

int totCatch(){ return Catch.index - 1; }

const char * gpsCatch( const int index ){
  return ( index > 0 && index < Catch.index ) ? Catch.ptr[ index ] : 0;
}

int lenCatch( const int index ){
  return ( index > 0 && index < Catch.index ) ? Catch.len[ index ] : 0;
}

char * cpyCatch( char * str, const int index ){
  if( index > 0 && index < Catch.index )
    strnCpy( str, Catch.ptr[ index ], Catch.len[ index ] );
  else *str = '\0';

  return str;
}

char * rplCatch( char * newStr, const char * rplStr, const int id ){
  char *oNewStr = newStr;
  const char *last = Catch.ptr[ 0 ];

  for( int index = 1, rpLen = strLen( rplStr ); index < Catch.index; index++ )
    if( id == Catch.id[ index ] ){
      if( last > Catch.ptr[index] ) last = Catch.ptr[index];

      strnCpy( newStr, last, Catch.ptr[index] - last );
      newStr += Catch.ptr[index] - last;
      strCpy( newStr, rplStr );
      newStr += rpLen;
      last    = Catch.ptr[index] + Catch.len[index];
    }

  strnCpy( newStr, last, Catch.ptr[0] + Catch.len[0] - last );
  return oNewStr;
}

char * putCatch( char * newStr, const char * putStr ){
  char *oNewStr = newStr;

  while( *putStr )
    switch( *putStr ){
    case '#':
      if( *++putStr == '#' )
        *newStr++ = *putStr++;
      else {
        int index = aToi( putStr );
        cpyCatch( newStr, index );
        newStr += lenCatch( index );
        putStr += countCharDigits( putStr );
      } break;
    default : *newStr++ = *putStr++;
    }

  *newStr = '\0';

  return oNewStr;
}
