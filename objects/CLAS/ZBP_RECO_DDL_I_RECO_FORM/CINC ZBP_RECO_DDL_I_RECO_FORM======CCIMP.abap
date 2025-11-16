CLASS lhc_zreco_ddl_i_reco_form DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zreco_ddl_i_reco_form RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zreco_ddl_i_reco_form RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zreco_ddl_i_reco_form.

    METHODS print FOR MODIFY
      IMPORTING keys FOR ACTION zreco_ddl_i_reco_form~print RESULT result.

    METHODS send FOR MODIFY
      IMPORTING keys FOR ACTION zreco_ddl_i_reco_form~send RESULT result.

ENDCLASS.

CLASS lhc_zreco_ddl_i_reco_form IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD print.

    DATA : lt_cform TYPE TABLE OF zreco_gtout,
           ls_cform TYPE zreco_gtout.
    DATA : lv_pdf TYPE string.
    TRY.
        READ ENTITIES OF zreco_ddl_i_reco_form IN LOCAL MODE
               ENTITY zreco_ddl_i_reco_form
                ALL FIELDS WITH CORRESPONDING #( keys )
               RESULT DATA(found_data).

        LOOP AT keys INTO DATA(ls_keys).
          MOVE-CORRESPONDING ls_keys TO ls_cform.
          APPEND ls_cform TO lt_cform.
        ENDLOOP.


        DATA lo_zreco_common  TYPE REF TO zreco_common.
        CREATE OBJECT lo_zreco_common.
        lo_zreco_common->multi_sending(
        EXPORTING
        it_cform = lt_cform
        iv_output = ''
        IMPORTING
         ev_pdf = lv_pdf
        ).


      CATCH cx_root INTO DATA(lx_err).





    ENDTRY.



*    DATA : ls_data TYPE zreco_s_pdf_data."zreco_s_carihesapmutabakat_pdf.
*    ls_data-cari_adres = 'ASDASDA'.
*    ls_data-sirket_adres = 'ASDASDA'.
*    APPEND INITIAL LINE TO ls_data-table1.
*
*    TRY.
*        CALL TRANSFORMATION zreco_form_pdf_takip
*        SOURCE form = ls_data
*        RESULT XML DATA(lv_xml).
*
*
*
*      CATCH cx_root INTO DATA(lo_root).
*    ENDTRY.
*
*    DATA(lv_base64_data) = cl_web_http_utility=>encode_x_base64( unencoded = lv_xml ).
*
*
*    TRY.
*        zcldobj_cl_ads_util=>call_adobe(
*          EXPORTING
*            iv_form_name            = 'ZETR_DECO_AF_CARIHESAPMUT'
*            iv_template_name        = 'CARIHESAPMUTABAKATI'
*            iv_xml                  = lv_base64_data "base64 verisi
*            iv_adobe_scenario       = 'ZCLDOBJ_CS_ADS'
*            iv_adobe_system         = 'ZCLDOBJ_CSYS_ADS'
*            iv_adobe_service_id     = 'ZCLDOBJ_OS_ADS_REST'
*          IMPORTING
*            ev_pdf                  = DATA(lv_pdf)
*            ev_response_code        = DATA(lv_res_c)
*            ev_response_text        = DATA(lv_res_t)
*        ).
*      CATCH cx_http_dest_provider_error.
*        "handle exception
*    ENDTRY.





    IF lv_pdf IS NOT INITIAL.
      result = VALUE #( ( %cid_ref = VALUE #( keys[ 1 ]-%cid_ref OPTIONAL )
                          akont = VALUE #( keys[ 1 ]-akont OPTIONAL )
                          bukrs = VALUE #( keys[ 1 ]-bukrs OPTIONAL )
                          gjahr = VALUE #( keys[ 1 ]-gjahr OPTIONAL )
                          period = VALUE #( keys[ 1 ]-period OPTIONAL )
                          uuid = VALUE #( keys[ 1 ]-uuid OPTIONAL )
                          waers = VALUE #( keys[ 1 ]-waers OPTIONAL )
                          %param-pdf = lv_pdf ) ).
    ENDIF.



  ENDMETHOD.

  METHOD send.

    DATA : lt_cform TYPE TABLE OF zreco_gtout,
           ls_cform TYPE zreco_gtout.

    TRY.

        READ ENTITIES OF zreco_ddl_i_reco_form IN LOCAL MODE
               ENTITY zreco_ddl_i_reco_form
                ALL FIELDS WITH CORRESPONDING #( keys )
               RESULT DATA(found_data).

        LOOP AT keys INTO DATA(ls_keys).
          MOVE-CORRESPONDING ls_keys TO ls_cform.
          APPEND ls_cform TO lt_cform.
        ENDLOOP.


        DATA lo_zreco_common  TYPE REF TO zreco_common.
        CREATE OBJECT lo_zreco_common.



        lo_zreco_common->multi_sending(
        it_cform = lt_cform
        iv_output = 'X'
         ).

        READ ENTITIES OF zreco_ddl_i_reco_form IN LOCAL MODE
    ENTITY zreco_ddl_i_reco_form
    FROM CORRESPONDING #( keys )
    RESULT DATA(foundS).

        LOOP AT foundS INTO DATA(found).
*          MODIFY ENTITIES OF zreco_ddl_i_reco_form IN LOCAL MODE
*                 ENTITY zreco_ddl_i_reco_form
*                 UPDATE FROM VALUE #( ( bukrs               = found-bukrs
*                                        %control-description = if_abap_behv=>mk-on ) ).


          MODIFY ENTITIES OF zreco_ddl_i_reco_form IN LOCAL MODE
              ENTITY zreco_ddl_i_reco_form
              UPDATE FROM VALUE #(
                (
                  %tky     = found-%tky
                  %data    = CORRESPONDING zreco_ddl_i_reco_form( found )
                      %control = VALUE #(
                                bukrs     = if_abap_behv=>mk-on
                                akont     = if_abap_behv=>mk-on
                                uuid      = if_abap_behv=>mk-on
                                period    = if_abap_behv=>mk-on
                                gjahr     = if_abap_behv=>mk-on
                                waers     = if_abap_behv=>mk-on
                                xsum      = if_abap_behv=>mk-on
                                ltext     = if_abap_behv=>mk-on
                                hesap_tur = if_abap_behv=>mk-on
                                hesap_no  = if_abap_behv=>mk-on
                                name1     = if_abap_behv=>mk-on
                                gsber     = if_abap_behv=>mk-on
                                ktokl     = if_abap_behv=>mk-on
                                dmbtr     = if_abap_behv=>mk-on
                                kursf     = if_abap_behv=>mk-on
                                wrbtr     = if_abap_behv=>mk-on
                                vkn_tckn  = if_abap_behv=>mk-on
                                vd        = if_abap_behv=>mk-on
                                spras     = if_abap_behv=>mk-on
                                telf1     = if_abap_behv=>mk-on
                                telf2     = if_abap_behv=>mk-on
                                telfx     = if_abap_behv=>mk-on
                                email     = if_abap_behv=>mk-on
                                kep_use   = if_abap_behv=>mk-on
                                kep_mail  = if_abap_behv=>mk-on
                                merge     = if_abap_behv=>mk-on
                                kunnr     = if_abap_behv=>mk-on
                                lifnr     = if_abap_behv=>mk-on
                                xausz     = if_abap_behv=>mk-on
                                no_local_curr = if_abap_behv=>mk-on
                                umskz     = if_abap_behv=>mk-on
                                p_daily   = if_abap_behv=>mk-on
                                rdate     = if_abap_behv=>mk-on
                                p_gsber   = if_abap_behv=>mk-on
                                p_waers   = if_abap_behv=>mk-on
                                p_seld    = if_abap_behv=>mk-on
                                ktokd     = if_abap_behv=>mk-on
                                dkont     = if_abap_behv=>mk-on
                                vkn_cr    = if_abap_behv=>mk-on
                                p_selk    = if_abap_behv=>mk-on
                                ktokk     = if_abap_behv=>mk-on
                                kkont     = if_abap_behv=>mk-on
                                brsch1    = if_abap_behv=>mk-on
                                brsch2    = if_abap_behv=>mk-on
                                vkn_ve    = if_abap_behv=>mk-on
                                p_tran    = if_abap_behv=>mk-on
                                p_all     = if_abap_behv=>mk-on
                                p_blist   = if_abap_behv=>mk-on
                                p_diff    = if_abap_behv=>mk-on
                                p_last    = if_abap_behv=>mk-on
                                p_cred    = if_abap_behv=>mk-on
                                p_print   = if_abap_behv=>mk-on
                                limit     = if_abap_behv=>mk-on
                                p_dmbtr   = if_abap_behv=>mk-on
                                p_shk     = if_abap_behv=>mk-on
                                p_date    = if_abap_behv=>mk-on
                                p_bli     = if_abap_behv=>mk-on
                                p_bsiz    = if_abap_behv=>mk-on
                                p_exch    = if_abap_behv=>mk-on
                                p_zero    = if_abap_behv=>mk-on
                                p_sgli    = if_abap_behv=>mk-on
                                s_sgli    = if_abap_behv=>mk-on
                                s_og      = if_abap_behv=>mk-on
                                p_novl    = if_abap_behv=>mk-on
                                p_nolc    = if_abap_behv=>mk-on
                                p_smkod   = if_abap_behv=>mk-on
                                p_salma   = if_abap_behv=>mk-on
                              )
                )
              ).

          INSERT VALUE #(
         %param = found
      ) INTO TABLE result.


        ENDLOOP.

      CATCH cx_root INTO DATA(lx_err).


    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZRECO_DDL_I_RECO_FORM DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZRECO_DDL_I_RECO_FORM IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.