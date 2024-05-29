*&---------------------------------------------------------------------*
*& Report  ztest_code_search
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ztest_code_search.

DATA(test_class) = NEW zrr_adcoset_tests( ).

test_class->run_test_data( ).
NEW-LINE.

*test_class->search_package_no_filter( ).
*new-LINE.
*test_class->search_package_no_filter( ).

*test_class->search_package_type_bdef( ).
*test_class->search_package_type_clas( ).
*test_class->search_package_type_fugr( ).
*test_class->search_package_type_ddls( ).
*test_class->search_package_type_dcls( ).
*test_class->search_package_type_ddlx( ).
*test_class->search_package_type_dtab( ).
*test_class->search_package_type_intf( ).
*test_class->search_package_type_prog( ).
*test_class->search_package_type_stru( ).
*test_class->search_package_type_type( ).
*test_class->search_package_type_xslt( ).

WRITE 'Test done'.
