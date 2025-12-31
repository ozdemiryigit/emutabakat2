  METHOD check_bsad.

    SELECT SINGLE *
    FROM I_OperationalAcctgDocItem
    INNER JOIN I_OplAcctgDocItemClrgHist
    ON I_OperationalAcctgDocItem~CompanyCode = I_OplAcctgDocItemClrgHist~ClearedCompanyCode
    AND I_OperationalAcctgDocItem~AccountingDocument = I_OplAcctgDocItemClrgHist~ClearedAccountingDocument
    AND I_OperationalAcctgDocItem~FiscalYear = I_OplAcctgDocItemClrgHist~ClearedFiscalYear
    AND I_OperationalAcctgDocItem~AccountingDocumentItem = I_OplAcctgDocItemClrgHist~ClearedAccountingDocumentItem
        WHERE I_OperationalAcctgDocItem~CompanyCode IN @s_bukrs  AND
              I_OperationalAcctgDocItem~Customer EQ @iv_kunnr AND
                  ( I_OperationalAcctgDocItem~PostingDate LE @gv_last_date AND I_OperationalAcctgDocItem~PostingDate GE @iv_budat ) AND
                  I_OperationalAcctgDocItem~SpecialGLCode IN @r_umskz_m AND
                  I_OplAcctgDocItemClrgHist~FinancialAccountType = 'D'
                  INTO @DATA(ls_bsad) .

    cv_closing_rc = sy-subrc.

  ENDMETHOD.