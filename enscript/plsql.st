/**
 * Name: plsql
 * Description: Oracle 10g SQL.
 * Author: Matt Bisson <bisson_matt@yahoo.com>
 */

state plsql_comment
{
  /\\\/\*/ {
    language_print ($0);
    call (plsql_comment);
  }
  /\*\\\// {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
   language_print ($0);
  }
}

state plsql_string
{
  /\\\\./ {
    language_print ($0);
  }
  /[\']/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state plsql
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (plsql_comment);
    comment_face (false);
  }
  /(\-\-.*)(\n)/ {
    comment_face (true);
    language_print ($1);
    comment_face (false);
    language_print ($2);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Character constants. */
  /[\']/ {
    string_face (true);
    language_print ($0);
    call (plsql_string);
    string_face (false);
  }

  /* Predefined SQL data */
  /\b(\
DBMS_OUTPUT\
|SQL\
)\b/i {
    variable_name_face (true);
    language_print ($0);
    variable_name_face (false);
  }

  /* Built-in functions.
   */
  /([\.][ \t]*)(\
COUNT\
|DELETE\
|EXISTS\
|EXTEND\
|FIRST\
|LAST\
|LIMIT\
|NEXT\
|PRIOR\
|TRIM\
)\b/ {
    language_print($1);
    builtin_face (true);
    language_print($2);
    builtin_face (false);
  }
  /([\%][ \t]*)(\
ROWCOUNT\
)\b/i {
    language_print($1);
    builtin_face (true);
    language_print($2);
    builtin_face (false);
  }
  /\b(\
ABS\
|ADD_MONTHS\
|ASCII\
|AVG\
|CEIL\
|CHARTOROWID\
|CHR\
|CONCAT\
|CONVERT\
|COS\
|COSH\
|COUNT\
|CURRVAL\
|DECODE\
|DUMP\
|EXP\
|FLOOR\
|GLB\
|GREATEST\
|GREATEST_LB\
|HEXTORAW\
|INITCAP\
|INSTR\
|INSTRB\
|LAST_DAY\
|LEAST\
|LEAST_UB\
|LENGTH\
|LENGTHB\
|LN\
|LOG\
|LOWER\
|LPAD\
|LTRIM\
|LUB\
|MAX\
|MIN\
|MOD\
|MONTHS_BETWEEN\
|NEW_TIME\
|NEXT_DAY\
|NEXTVAL\
|NLS_INITCAP\
|NLS_LOWER\
|NLS_UPPER\
|NLSSORT\
|NVL\
|POWER\
|RAWTOHEX\
|REPLACE\
|ROUND\
|ROWIDTOCHAR\
|RPAD\
|RTRIM\
|SIGN\
|SIN\
|SINH\
|SOUNDEX\
|SQLCODE\
|SQLERRM\
|SQRT\
|STDDEV\
|SUM\
|SUBSTR\
|SUBSTRB\
|TAN\
|TANH\
|TO_CHAR\
|TO_DATE\
|TO_LABEL\
|TO_MULTI_BYTE\
|TO_NUMBER\
|TO_SINGLE_BYTE\
|TRANSLATE\
|TRIM\
|TRUNC\
|UID\
|UPPER\
|USERENV\
|VALUE\
|VARIANCE\
|VSIZE\
)\b([ \t]*\()/i {
    builtin_face (true);
    language_print($1);
    builtin_face (false);
    language_print($2);
  }

  /* Datatypes.
   */
  /([\%][ \t]*)(\
ROWTYPE\
|TYPE\
)\b/i {
    language_print($1);
    builtin_face (true);
    language_print($2);
    builtin_face (false);
  }
  /\b(\
ARRAY\
|BINARY_INTEGER\
|BOOLEAN\
|CHAR\
|CHAR_BASE\
|CURSOR\
|DATE\
|DECIMAL\
|FUNCTION\
|INTEGER\
|INTERFACE\
|INTERVAL\
|MINUTE\
|MLSLABEL\
|NATURAL\
|NATURALN\
|NUMBER\
|NUMBER_BASE\
|OCIROWID\
|PACKAGE\
|PLS_INTEGER\
|POSITIVE\
|POSITIVEN\
|PROCEDURE\
|RANGE\
|REAL\
|RECORD\
|REF\
|ROW\
|ROWID\
|ROWTYPE\
|SMALLINT\
|SYSDATE\
|TABLE\
|TIME\
|TIMESTAMP\
|TIMEZONE_ABBR\
|TIMEZONE_HOUR\
|TIMEZONE_MINUTE\
|TIMEZONE_REGION\
|TYPE\
|VARCHAR\
|VARCHAR2\
|VARIANCE\
|VIEW\
)\b/i {
    type_face (true);
    language_print ($0);
    type_face (false);
  }

  /* Keywords.
   */
  /\b(\
ACCESS\
|ADD\
|ALLA\
|ALTER\
|AND\
|ANY\
|AS\
|ASC\
|AT\
|AUDIT\
|AUTHID\
|AVG\
|BEGIN\
|BETWEEN\
|BODY\
|BULK\
|BY\
|CAST\
|CHECK\
|CLOSE\
|CLUSTER\
|COALESCE\
|COLLECT\
|COLUMN\
|COMMENT\
|COMMIT\
|COMPRESS\
|CONNECT\
|CONSTANT\
|CREATE\
|CURRENT\
|CURRVAL\
|DAY\
|DECLARE\
|DEFAULT\
|DELETE\
|DESC\
|DISTINCT\
|DO\
|DROP\
|ELSE\
|ELSIF\
|END\
|EXCEPTION\
|EXCLUSIVE\
|EXECUTE\
|EXISTS\
|EXIT\
|EXTENDS\
|EXTRACT\
|FALSE\
|FETCH\
|FILE\
|FLOAT\
|FOR\
|FORALL\
|FROM\
|GOTO\
|GRANT\
|GROUP\
|HAVING\
|HEAP\
|HOUR\
|IDENTIFIED\
|IF\
|IMMEDIATE\
|IN\
|INCREMENT\
|INDEX\
|INDICATOR\
|INITIAL\
|INSERT\
|INTERSECT\
|INTO\
|IS\
|ISOLATION\
|JAVA\
|LEVEL\
|LIKE\
|LIMITED\
|LOCK\
|LONG\
|LOOP\
|MAX\
|MAXEXTENTS\
|MIN\
|MINUS\
|MOD\
|MODE\
|MODIFY\
|MONTH\
|NEW\
|NEXTVAL\
|NOAUDIT\
|NOCOMPRESS\
|NOCOPY\
|NOT\
|NOWAIT\
|NULL\
|NULLIF\
|OF\
|OFFLINE\
|ON\
|ONLINE\
|OPAQUE\
|OPEN\
|OPERATOR\
|OPTION\
|OR\
|ORDER\
|ORGANIZATION\
|OTHERS\
|OUT\
|PARTITION\
|PCTFREE\
|PRAGMA\
|PRIOR\
|PRIVATE\
|PRIVILEGES\
|PUBLIC\
|RAISE\
|RAW\
|RELEASE\
|RENAME\
|RESOURCE\
|REPLACE\
|RETURN\
|REVERSE\
|REVOKE\
|ROLLBACK\
|ROWNUM\
|ROWS\
|SAVEPOINT\
|SECOND\
|SELECT\
|SEPERATE\
|SESSION\
|SET\
|SHARE\
|SIZE\
|SPACE\
|SQL\
|SQLCODE\
|SQLERRM\
|START\
|STDDEV\
|SUBTYPE\
|SUCCESSFUL\
|SUM\
|SYNONYM\
|THEN\
|TO\
|TRIGGER\
|TRUE\
|UI\
|UID\
|UNION\
|UNIQUE\
|UPDATE\
|USE\
|USER\
|VALIDATE\
|VALUES\
|VARYING\
|WHEN\
|WHENEVER\
|WHERE\
|WHILE\
|WITH\
|WORK\
|WRITE\
|YEAR\
|ZONE\
)\b/i {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}
