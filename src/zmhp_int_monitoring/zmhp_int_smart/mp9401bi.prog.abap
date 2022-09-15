*&---------------------------------------------------------------------*
*& Report  MP9401BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9401bi.

TABLES: p9401.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9401.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9401 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9401-ORACLE_ID_INTERNAL'
  p9401-oracle_id_internal.
  PERFORM fill_field(rhaltd00) USING
  'P9401-ORACLEID'
  p9401-oracleid.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9401-DUMMY' P9401-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
