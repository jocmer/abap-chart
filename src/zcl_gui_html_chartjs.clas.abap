class ZCL_GUI_HTML_CHARTJS definition
  public
  inheriting from CL_GUI_HTML_VIEWER
  create public .

public section.

  types TYPE_DATA type INT4 .
  types:
    type_data_t TYPE TABLE OF type_data WITH DEFAULT KEY .
  types TYPE_LABEL type STRING .
  types:
    type_label_t TYPE TABLE OF type_label WITH DEFAULT KEY .
  types:
    type_string_t TYPE TABLE OF string WITH DEFAULT KEY .
  types:
    BEGIN OF type_dataset,
             id                           TYPE char32,
             label                        TYPE string,
             data                         TYPE type_data_t,
             background_color             TYPE type_string_t,
             border_cap_style             TYPE string,
             border_color                 TYPE type_string_t,
             border_dash                  TYPE string,
             border_dash_offset           TYPE string,
             border_join_style            TYPE string,
             border_width                 TYPE int4,
             hover_background_color       TYPE type_string_t,
             hover_border_color           TYPE type_string_t,
             point_background_color       TYPE string,
             point_border_color           TYPE string,
             point_border_width           TYPE i,
             point_hover_background_color TYPE string,
             point_hover_border_color     TYPE string,
             point_hover_border_width           TYPE i,
             fill                         TYPE abap_bool,
             tension                      TYPE string,
             type                         TYPE string,
             show_line                    TYPE abap_bool,
             span_gaps                    TYPE abap_bool,
             order                        TYPE i,
           END OF type_dataset .
  types:
    type_dataset_t TYPE TABLE OF type_dataset .
  types:
    BEGIN OF type_legend,
             position TYPE string,
           END OF type_legend .
  types:
    BEGIN OF type_padding,
             top    TYPE int4,
             left   TYPE int4,
             bottom TYPE int4,
             right  TYPE int4,
           END OF type_padding .
  types:
    BEGIN OF type_font,
             family TYPE string,
             size   TYPE int4,
             weight TYPE string,
           END OF type_font .
  types:
    BEGIN OF type_title,
             align    TYPE string,
             color    TYPE string,
             display  TYPE abap_bool,
             fullsize TYPE abap_bool,
             position TYPE string,
             padding  TYPE type_padding,
             text     TYPE string,
             font     TYPE type_font,
           END OF type_title .
  types:
    BEGIN OF type_ticks,
             step_size TYPE int4,
             color     TYPE string,
             align     TYPE string,
           END OF type_ticks .
  types:
    BEGIN OF type_grid,
             display            TYPE abap_bool,
             draw_border        TYPE abap_bool,
             draw_on_chart_area TYPE abap_bool,
             draw_ticks         TYPE abap_bool,
           END OF type_grid .
  types:
    BEGIN OF type_scale_y,
             display       TYPE abap_bool,
             title         TYPE type_title,
             begin_at_zero TYPE abap_bool,
             min           TYPE int4,
             max           TYPE int4,
             ticks         TYPE type_ticks,
             suggested_min TYPE int4,
             suggested_max TYPE int4,
             type          TYPE string,
             position      TYPE string,
             grid          TYPE type_grid,
             stacked       TYPE abap_bool,
           END OF type_scale_y .
  types:
    BEGIN OF type_scale_x,
             display  TYPE abap_bool,
             title    TYPE type_title,
             position TYPE string,
             grid     TYPE type_grid,
           END OF type_scale_x .
  types:
    BEGIN OF type_scales,
             y TYPE type_scale_y,
             x TYPE type_scale_x,
           END OF type_scales .
  types:
    BEGIN OF type_tooltip,
             mode      TYPE string,
             intersect TYPE abap_bool,
           END OF type_tooltip .
  types:
    BEGIN OF type_plugins,
             title    TYPE type_title,
             subtitle TYPE type_title,
             tooltip  TYPE type_tooltip,
             legend   TYPE type_legend,
           END OF type_plugins .
  types:
    BEGIN OF type_interaction,
             mode      TYPE string,
             axis      TYPE string,
             intersect TYPE abap_bool,
           END OF type_interaction .
  types:
    BEGIN OF type_hover,
             mode      TYPE string,
             intersect TYPE abap_bool,
           END OF type_hover .
  types:
    BEGIN OF type_options,
             responsive  TYPE abap_bool,
             index_axis  TYPE string,
             hover       TYPE type_hover,
             scales      TYPE type_scales,
             plugins     TYPE type_plugins,
             interaction TYPE type_interaction,
           END OF type_options .

  constants:
    BEGIN OF gc_bool,
                 false TYPE string VALUE 'false',
                 true  TYPE string VALUE abap_true,
               END OF gc_bool .

  methods CONSTRUCTOR
    importing
      !IR_PARENT type ref to CL_GUI_CONTAINER
    exceptions
      CNTL_ERROR
      CNTL_INSTALL_ERROR
      DP_INSTALL_ERROR
      DP_ERROR .
  methods INIT_DISPLAY .
  methods REFRESH .
  methods SET_CHART_TYPE
    importing
      !IV_CHART_TYPE type STRING .
  methods SET_OPTIONS
    importing
      !IS_OPTIONS type TYPE_OPTIONS .
  methods GET_OPTIONS
    returning
      value(RS_OPTIONS) type TYPE_OPTIONS .
  methods SET_LABELS
    importing
      !IT_LABELS type TYPE_LABEL_T .
  methods GET_LABELS
    returning
      value(RT_LABELS) type TYPE_LABEL_T .
  methods ADD_DATASET
    importing
      !IS_DATASET type TYPE_DATASET .
  methods SET_DATASETS
    importing
      !IT_DATASETS type TYPE_DATASET_T .
  methods MODIFY_DATASET
    importing
      !IV_ID type STRING
      !IS_DATASET type TYPE_DATASET
    exceptions
      INVALID_ID .
  methods DELETE_DATASET
    importing
      !IV_ID type STRING .
  methods DELETE_DATASETS .
  methods GET_DATASET
    importing
      !IV_ID type STRING
    exporting
      !ES_DATASET type TYPE_DATASET .
  methods GET_DATASETS
    exporting
      !ET_DATASETS type TYPE_DATASET_T .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_charttype TYPE string.
    DATA: gt_labels TYPE type_label_t.
    DATA: gt_datasets TYPE type_dataset_t.
    DATA: gs_option TYPE type_options.

    METHODS load_document .
    METHODS on_sapevent
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !frame
          !getdata
          !postdata
          !query_table .
    METHODS number_to_string
      IMPORTING
        !iv_number       TYPE i
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS bool_to_string
      IMPORTING
        !iv_bool         TYPE abap_bool
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS unicode_convert
      IMPORTING
        !iv_line       TYPE string
      RETURNING
        VALUE(rv_line) TYPE string .
    METHODS defaults
      CHANGING
        cs_options TYPE type_options.

ENDCLASS.



CLASS ZCL_GUI_HTML_CHARTJS IMPLEMENTATION.


  METHOD add_dataset.
    APPEND is_dataset TO gt_datasets.
  ENDMETHOD.


  METHOD bool_to_string.
    IF iv_bool = abap_true.
      rv_string = 'true'.
    ELSE.
      rv_string = 'false'.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        parent             = ir_parent
        uiflag             = 4
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    CASE sy-subrc.
      WHEN 1.
        RAISE cntl_error.
      WHEN 2.
        RAISE cntl_install_error.
      WHEN 3.
        RAISE dp_install_error.
      WHEN 4.
        RAISE dp_error.
    ENDCASE.

    CALL METHOD load_mime_object
      EXPORTING
        object_id  = 'Z_CHARTJS'
        object_url = 'chart.min.js'
      EXCEPTIONS
        OTHERS     = 1.

    DATA(ls_events) = VALUE cntl_simple_event( eventid = me->m_id_sapevent appl_event = abap_true ).
    DATA: lt_events TYPE cntl_simple_events.
    APPEND ls_events TO lt_events.

    set_registered_events(
      events = lt_events
    ).

    SET HANDLER on_sapevent FOR me.

  ENDMETHOD.


  METHOD defaults.



  ENDMETHOD.


  METHOD delete_dataset.

    DELETE gt_datasets WHERE id = iv_id.

  ENDMETHOD.


  METHOD delete_datasets.
    FREE: gt_datasets.
  ENDMETHOD.


  METHOD get_dataset.
    READ TABLE gt_datasets
        INTO es_dataset
        WITH KEY id = iv_id.
  ENDMETHOD.


  METHOD get_datasets.
    et_datasets = gt_datasets.
  ENDMETHOD.


  METHOD get_labels.
    rt_labels = gt_labels.
  ENDMETHOD.


  METHOD get_options.
    rs_options = gs_option.
  ENDMETHOD.


  METHOD init_display.
    load_document(  ).
  ENDMETHOD.


  METHOD load_document.
    DATA: lv_doc_url(80).
    CALL METHOD me->load_html_document
      EXPORTING
        document_id  = 'Z_HTML_CHARTJS'
      IMPORTING
        assigned_url = lv_doc_url
      EXCEPTIONS
        OTHERS       = 1.

    DATA: lv_key TYPE wwwdatatab.
    lv_key = 'HTZ_HTML_CHARTJS'.

    DATA: lt_html TYPE TABLE OF w3html.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = lv_key
      TABLES
        html   = lt_html
      EXCEPTIONS
        OTHERS = 1.

*    DATA: lt_text TYPE STANDARD TABLE OF char4000 INITIAL SIZE 200.
    DATA: lt_text TYPE type_string_t.
    LOOP AT lt_html INTO DATA(ls_html).
      DATA: lv_text TYPE string. "LIKE ls_html-line.
      MOVE ls_html-line TO lv_text.
      APPEND lv_text TO lt_text.
    ENDLOOP.

    DATA: lv_string TYPE string.
    LOOP AT lt_text INTO lv_text.
      DATA(lv_tabix) = sy-tabix.

      REPLACE '<!-- CHART_TYPE -->' IN lv_text WITH gv_charttype.
      IF sy-subrc = 0.
        MODIFY lt_text FROM lv_text INDEX lv_tabix.
      ENDIF.

      FIND '<!-- LABELS -->' IN lv_text.
      IF sy-subrc = 0.
        DATA(lv_json) = /ui2/cl_json=>serialize(
            compress = abap_true
            data = gt_labels
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        REPLACE '<!-- LABELS -->' IN lv_text WITH lv_json.
        MODIFY lt_text FROM lv_text INDEX lv_tabix.
      ENDIF.

      FIND '<!-- DATASETS -->' IN lv_text.
      IF sy-subrc = 0.
        lv_json = /ui2/cl_json=>serialize(
            compress = abap_true
            data = gt_datasets
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        REPLACE '<!-- DATASETS -->' IN lv_text WITH lv_json.
        MODIFY lt_text FROM lv_text INDEX lv_tabix.
      ENDIF.

      FIND '<!-- OPTIONS -->' IN lv_text.
      IF sy-subrc = 0.
        lv_json = /ui2/cl_json=>serialize(
        compress = abap_true
            data = gs_option
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        REPLACE '<!-- OPTIONS -->' IN lv_text WITH lv_json.
        MODIFY lt_text FROM lv_text INDEX lv_tabix.
      ENDIF.


    ENDLOOP.

    LOOP AT lt_text INTO lv_text.
*      REPLACE '<!-- BGCOLOR -->' IN lv_text WITH lv_bgcolor.
*      REPLACE '<!-- FLASH -->' IN lv_text WITH CONV char255( iv_flash_path ).
*      REPLACE '<!-- DATA -->' IN lv_text WITH iv_data_path.
*      REPLACE '<!-- WIDTH -->' IN lv_text WITH iv_flash_width.
*      REPLACE '<!-- HEIGHT -->' IN lv_text WITH iv_flash_height.
*
      lv_text = unicode_convert( lv_text ).
      MODIFY lt_text FROM lv_text INDEX sy-tabix.
    ENDLOOP.

    DATA: lt_html2 TYPE TABLE OF char120.

    LOOP AT lt_text INTO lv_string.

      CALL FUNCTION 'SWA_STRING_TO_TABLE'
        EXPORTING
          character_string           = lv_string
          append                     = 'X'
          line_size                  = 120
*         CHECK_TABLE_TYPE           = ' '
        IMPORTING
          character_table            = lt_html2
*         TOTAL_LENGTH               =
*         LINE_SIZE_USED             =
*         LINES_FILLED               =
*         LAST_LINE_LENGTH           =
        EXCEPTIONS
          no_flat_charlike_structure = 1
          OTHERS                     = 2.

    ENDLOOP.

    CALL METHOD me->load_data
      EXPORTING
        url          = 'Z_BASE_CHART'
        type         = 'text'
        subtype      = 'html'
      IMPORTING
        assigned_url = lv_doc_url
      CHANGING
        data_table   = lt_html2.
*        data_table   = lt_text.


    IF sy-subrc EQ 0.

      CALL METHOD me->show_url
        EXPORTING
          url = lv_doc_url. "CONV char255( IV_FLASH_PATH ).

    ENDIF.
  ENDMETHOD.


  METHOD modify_dataset.

    READ TABLE gt_datasets REFERENCE INTO DATA(lr_dataset)
        WITH KEY id = iv_id.
    IF sy-subrc <> 0.
      RAISE invalid_id.
    ENDIF.

    MOVE-CORRESPONDING is_dataset TO lr_dataset->*.

  ENDMETHOD.


  METHOD number_to_string.
    DATA: lv_char TYPE char32.
    WRITE: iv_number TO lv_char.
    CONDENSE lv_char.
    REPLACE '.' IN lv_char WITH ''.
    rv_string = lv_char.
  ENDMETHOD.


  METHOD on_sapevent.
    DATA: lv_p1 TYPE string,
          lv_p2 TYPE string.
    SPLIT getdata AT '=' INTO lv_p1 lv_p2.

    CASE action.
      WHEN ''.

    ENDCASE.
  ENDMETHOD.


  METHOD refresh.
    load_document(  ).
  ENDMETHOD.


  METHOD set_chart_type.
    gv_charttype = iv_chart_type.
  ENDMETHOD.


  METHOD set_datasets.
    gt_datasets = it_datasets.
  ENDMETHOD.


  METHOD set_labels.
    gt_labels = it_labels.
  ENDMETHOD.


  METHOD set_options.
    gs_option = is_options.
  ENDMETHOD.


  METHOD unicode_convert.

*   Unicode konvertierung für HTML Dokumente
    rv_line  = iv_line.

*    REPLACE ALL OCCURRENCES OF '&uuml;' IN rv_line WITH 'ü'.
*    REPLACE ALL OCCURRENCES OF '&ouml;' IN rv_line WITH 'ö'.
*    REPLACE ALL OCCURRENCES OF '&auml;' IN rv_line WITH 'ä'.
*
*    REPLACE ALL OCCURRENCES OF '&' IN rv_line WITH '&amp;'.

*    REPLACE ALL OCCURRENCES OF 'ä' IN rv_line WITH '&auml;'.
*    REPLACE ALL OCCURRENCES OF 'ö' IN rv_line WITH '&ouml;'.
*    REPLACE ALL OCCURRENCES OF 'ü' IN rv_line WITH '&uuml;'.
*    REPLACE ALL OCCURRENCES OF 'Ä' IN rv_line WITH '&Auml;'.
*    REPLACE ALL OCCURRENCES OF 'Ö' IN rv_line WITH '&Ouml;'.
*    REPLACE ALL OCCURRENCES OF 'Ü' IN rv_line WITH '&Uuml;'.

*    REPLACE ALL OCCURRENCES OF 'ß' IN rv_line WITH '&szlig;'.
*    REPLACE ALL OCCURRENCES OF '€' IN rv_line WITH '&euro;'.
*    REPLACE ALL OCCURRENCES OF '§' IN rv_line WITH '&sect;'.

  ENDMETHOD.
ENDCLASS.
