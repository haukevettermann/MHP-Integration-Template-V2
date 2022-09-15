*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9400                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP940000 MESSAGE-ID RP.

TABLES: P9400.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9400
                       DEFAULT P9400.

DATA: PSAVE LIKE P9400.
