  METHOD check_bsid.
    SELECT SINGLE *
       FROM zetr_reco_ddl_bsid
           WHERE CompanyCode IN @s_bukrs  AND
                 Customer EQ @iv_kunnr AND
                     ( PostingDate LE @gv_last_date AND PostingDate GE @iv_budat ) AND
                     SpecialGLCode IN @r_umskz_m
                     INTO @DATA(ls_bsid) .

    cv_closing_rc = sy-subrc.

  ENDMETHOD.