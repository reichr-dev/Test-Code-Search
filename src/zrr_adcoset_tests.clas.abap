CLASS zrr_adcoset_tests DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS run_test_data.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF test_data,
        package_filter   TYPE string,
        type_filter      TYPE string,
        match_count      TYPE i,
        searched_objects TYPE i,
        searched_sources TYPE i,
        query_time       TYPE i,
      END OF test_data.

    DATA test_data_table TYPE TABLE OF test_data.

    METHODS convert_scope_params_request
      IMPORTING
        is_query_input TYPE zif_adcoset_ty_adt_types=>ty_search_scope_params
      RETURNING
        VALUE(result)  TYPE xstring.

    METHODS convert_scope_params_response
      IMPORTING
        iv_message_body TYPE xstring
      RETURNING
        VALUE(result)   TYPE zif_adcoset_ty_adt_types=>ty_search_scope.

    METHODS convert_search_result_response
      IMPORTING
        iv_message_body TYPE xstring
      RETURNING
        VALUE(result)   TYPE zif_adcoset_ty_adt_types=>ty_code_search_result.

    METHODS create_scope_request
      IMPORTING
        scope_input          TYPE zif_adcoset_ty_adt_types=>ty_search_scope_params
      RETURNING
        VALUE(scope_request) TYPE sadt_rest_request.

    METHODS create_search_request
      IMPORTING
        scope_offset          TYPE i
        scope_id              TYPE sysuuid_x16
      RETURNING
        VALUE(search_request) TYPE sadt_rest_request.

    METHODS execute_search
      IMPORTING
        scope_input      TYPE zif_adcoset_ty_adt_types=>ty_search_scope_params
      EXPORTING
        match_count      TYPE i
        searched_objects TYPE i
        searched_sources TYPE i
        query_time       TYPE i.
ENDCLASS.


CLASS zrr_adcoset_tests IMPLEMENTATION.
  METHOD constructor.
    test_data_table = VALUE #(
        package_filter = 'aba_gen'
        ( type_filter = ''     match_count = 1830 searched_objects = 7401 searched_sources = 60559 )
        ( type_filter = 'bdef' match_count = 0    searched_objects = 0    searched_sources = 0 )
        ( type_filter = 'clas' match_count = 826  searched_objects = 2534 searched_sources = 43770 )
        ( type_filter = 'dcls' match_count = 0    searched_objects = 0    searched_sources = 0 )
        ( type_filter = 'ddls' match_count = 0    searched_objects = 0    searched_sources = 0 )
        ( type_filter = 'ddlx' match_count = 0    searched_objects = 0    searched_sources = 0 )
        ( type_filter = 'fugr' match_count = 850  searched_objects = 1252 searched_sources = 13174 )
        ( type_filter = 'intf' match_count = 3    searched_objects = 942  searched_sources = 942 )
        ( type_filter = 'prog' match_count = 130  searched_objects = 2530 searched_sources = 2530 )
        ( type_filter = 'type' match_count = 0    searched_objects = 39   searched_sources = 39 )
        ( type_filter = 'xslt' match_count = 21   searched_objects = 104  searched_sources = 104 ) ).
  ENDMETHOD.

  METHOD convert_scope_params_request.
    DATA xml TYPE string.

    DATA(out_converter) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

    CALL TRANSFORMATION zadcoset_search_scope_params
         SOURCE scope_params = is_query_input
         RESULT XML xml.

    out_converter->convert( EXPORTING data   = xml
                            IMPORTING buffer = result ).
  ENDMETHOD.

  METHOD convert_scope_params_response.
    DATA lv_xml TYPE string.

    DATA(in_converter) = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).

    in_converter->convert( EXPORTING input = iv_message_body
                           IMPORTING data  = lv_xml ).

    TRY.
        CALL TRANSFORMATION zadcoset_search_scope
             SOURCE XML lv_xml
             RESULT scope = result.
      CATCH cx_root INTO DATA(lx_error). " TODO: variable is assigned but never used (ABAP cleaner)
        IF 1 = 2.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD convert_search_result_response.
    DATA lv_xml TYPE string.

    DATA(in_converter) = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).

    in_converter->convert( EXPORTING input = iv_message_body
                           IMPORTING data  = lv_xml ).

    TRY.
        CALL TRANSFORMATION zadcoset_search_result
             SOURCE XML lv_xml
             RESULT root = result.
      CATCH cx_root INTO DATA(lx_error). " TODO: variable is assigned but never used (ABAP cleaner)
        IF 1 = 2.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD create_scope_request.
    scope_request = VALUE sadt_rest_request(
                              request_line  = VALUE #( method = 'POST'
                                                       uri    = '/devepos/adt/cst/codesearch/scope' )
                              header_fields = VALUE #( ( name = 'Content-Type' value = 'application/xml' ) )
                              message_body  = convert_scope_params_request( scope_input ) ).
  ENDMETHOD.

  METHOD create_search_request.
    DATA(search_uri) = |/devepos/adt/cst/codesearch?searchPattern=ToDo&scopeId={ scope_id }&scopeOffset={ scope_offset }&maxObjects=100&classIncludes=all&fugrIncludes=all&ignoreCase=true|.

    search_request = VALUE sadt_rest_request( request_line = VALUE #( method = 'GET'
                                                                      uri    = search_uri ) ).
  ENDMETHOD.

  METHOD execute_search.
    DATA scope_offset TYPE i.

    DATA scope_response TYPE sadt_rest_response.
    DATA search_response TYPE sadt_rest_response.

    CLEAR: match_count,
           searched_objects,
           searched_sources,
           query_time.

    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING request  = create_scope_request( scope_input )
      IMPORTING response = scope_response.

    DATA(scope) = convert_scope_params_response( scope_response-message_body ).

    WHILE scope-object_count > scope_offset.
      "
      DATA(search_request) = create_search_request( scope_offset = scope_offset
                                                    scope_id     = scope-id ).

      CALL FUNCTION 'SADT_REST_RFC_ENDPOINT' DESTINATION 'NONE'
        EXPORTING request  = search_request
        IMPORTING response = search_response.

      DATA(search_result) = convert_search_result_response( search_response-message_body ).

      match_count      = match_count      + search_result-number_of_results.
      searched_objects = searched_objects + search_result-number_of_searched_objects.
      searched_sources = searched_sources + search_result-number_of_searched_sources.
      query_time       = query_time       + search_result-query_time_in_ms.

      IF scope_offset + 100 < scope-object_count.
        scope_offset = scope_offset + 100.
      ELSE.
        scope_offset = scope_offset + ( scope-object_count - scope_offset ).
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD run_test_data.
    DATA match_count TYPE i.
    DATA searched_objects TYPE i.
    DATA searched_sources TYPE i.
    DATA query_time TYPE i.

    LOOP AT test_data_table ASSIGNING FIELD-SYMBOL(<test_data>).

      CLEAR: match_count,
             searched_objects,
             searched_sources,
             query_time.

      DATA(scope_input) = VALUE zif_adcoset_ty_adt_types=>ty_search_scope_params(
                                    ( name = 'packageName' value = <test_data>-package_filter )
                                    ( name = 'objectType' value = <test_data>-type_filter )
                                    ( name = 'tagId' value = '' ) ).

      execute_search( EXPORTING scope_input      = scope_input
                      IMPORTING match_count      = match_count
                                searched_objects = searched_objects
                                searched_sources = searched_sources
                                query_time       = query_time ).

      WRITE: match_count,searched_objects, searched_sources, query_time.NEW-LINE.

      IF match_count <> <test_data>-match_count.
        WRITE: 'wrong match count for', <test_data>-type_filter.NEW-LINE.
      ENDIF.

      IF searched_objects <> <test_data>-searched_objects.
        WRITE: 'wrong searched objects for', <test_data>-type_filter.NEW-LINE.
      ENDIF.

      IF searched_sources <> <test_data>-searched_sources.
        WRITE: 'wrong searched searched_sources for', <test_data>-type_filter.NEW-LINE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
