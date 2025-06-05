&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/*[COMMENT]*/                    /* Connected Databases 
/*[COMMENT]*/                              buint            PROGRESS
/*[COMMENT]*/                    */
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*[COMMENT]*/                    /*************************************************************************
/*[COMMENT]*/                     WRSBQ7072.W : Queue ÃÑº¢éÍÁÙÅ¡ÃÁ¸ÃÃÁìÃ¶Â¹µì/¾Ãº.¨Ò¡ DB BUExt à¢éÒ BUInt
/*[COMMENT]*/                     Copyright   : Safety Insurance Public Company Limited
/*[COMMENT]*/                                   ºÃÔÉÑ· »ÃÐ¡Ñ¹¤ØéÁÀÑÂ ¨Ó¡Ñ´ (ÁËÒª¹)
/*[COMMENT]*/                    -------------------------------------------------------------------------
/*[COMMENT]*/                     Database    : BUExt + BUInt + sic_bran + stat + formtmp + gwctx + Expiry
/*[COMMENT]*/                    -------------------------------------------------------------------------
/*[COMMENT]*/                     CREATE BY   : sombat   ASSIGN: A57-0096    DATE: 30/06/2014
/*[COMMENT]*/                     Modify BY   : sombat   ASSIGN: A57-0300    DATE: 01/09/2014 
/*[COMMENT]*/                                 : ãËé´Ö§¢éÍÁÙÅ§Ò¹µèÍÍÒÂØ¨Ò¡ expiry
/*[COMMENT]*/                                 : ª×èÍÊ¡ØÅ ·ÕèÍÂÙè, ¼ÙéÃÑº¼Å»ÃÐâÂª¹ì ãËéÂÖ´µÒÁ¢éÍÁÙÅ·Õè¹ÓÊè§
/*[COMMENT]*/                                 : ÊèÇ¹·ÕèàËÅ×Í ÂÖ´µÒÁ¢éÍÁÙÅ expiry
/*[COMMENT]*/                     Modify BY   : sombat   ASSIGN: A58-0125    DATE: 23/03/2015
/*[COMMENT]*/                                   â»Ãá¡ÃÁÈÃÕ¡ÃØ§âºÃ¤à¡ÍÃì »ÃÐ¡Ñ¹ÀÑÂ ».2+ äÁèµéÍ§µÃÇ¨ÊÀÒ¾Ã¶ 
/*[COMMENT]*/                                   ÊÓËÃÑº·Ø¹·ÕèäÁèà¡Ô¹ 200,000 ºÒ·
/*[COMMENT]*/                     Modify By   : Kridtiya i. Date. 27/01/2018 Check blacklist  
/*[COMMENT]*/                     modify by   : kridtiya i. date. 03/09/2018 ãËéÃÑº¤èÒ¤Ó¹ÓµÒÁ»¡µÔ             
/*[COMMENT]*/                    *************************************************************************/
/*[COMMENT]*/                    /*------------------------------------------------------------------------
/*[BLANK]*/                      
/*[COMMENT]*/                      File: 
/*[BLANK]*/                      
/*[COMMENT]*/                      Description: 
/*[BLANK]*/                      
/*[COMMENT]*/                      Input Parameters:
/*[COMMENT]*/                          <none>
/*[COMMENT]*/                      Output Parameters:
/*[COMMENT]*/                          <none>
/*[COMMENT]*/                      Author: 
/*[COMMENT]*/                      Created: 
/*[BLANK]*/                        
/*[COMMENT]*/                      Database: 
/*[COMMENT]*/                    ------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*          This .W file was created with the Progress AppBuilder.      */
/*[COMMENT]*/                    /*----------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Create an unnamed pool to store all the widgets created 
/*[COMMENT]*/                         by this procedure. This is a good default which assures
/*[COMMENT]*/                         that this procedure's triggers and internal procedures 
/*[COMMENT]*/                         will execute in this procedure's storage, and that proper
/*[COMMENT]*/                         cleanup will occur on deletion of the procedure. */
/*[BLANK]*/                      
CREATE WIDGET-POOL.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***************************  Definitions  ************************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Parameters Definitions ---                                           */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Local Variable Definitions ---                                       */
DEFINE VARIABLE nv_ConfirmBy  AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_rec_rq     AS RECID                               NO-UNDO. /* RECORD ID REQUEST */
DEFINE VARIABLE nv_rec_rs     AS RECID                               NO-UNDO. /* RECORD ID RESPONSE */
DEFINE VARIABLE nv_process    AS CHARACTER                           NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_InsurerCodeRq      AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_ConfirmByUserID    AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_UserIDLine         AS INTEGER                             NO-UNDO.
DEFINE VARIABLE nv_FindUcf            AS INT64                               NO-UNDO.
DEFINE VARIABLE nv_recUcf             AS INT64                               NO-UNDO.
DEFINE VARIABLE nv_SwtFind            AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
DEFINE VARIABLE my-datetime           AS CHARACTER FORMAT "X(23)"            NO-UNDO. 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* 2555/09/01 Chek Number Limit Request / Companay*/
DEFINE VARIABLE nv_NumLimitRqPerDay   AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumLimitRqPerWeek  AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumLimitRqPerMonth AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumLimitRqPerYear  AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumRqPerDay        AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumRqPerWeek       AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumRqPerMonth      AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_NumRqPerYear       AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_ManageTo           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_SendTo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_ForDate            AS DATE      NO-UNDO.
DEFINE VARIABLE nv_Week               AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_Month              AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_count              AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_PolicyV70          AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_DocnoV70           AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_PolicyV72          AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_DocnoV72           AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_msgerror           AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_octets             AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_sendchkvehicle     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_cvehtext           AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_resulttext         AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_RecIntPol7072      AS RECID     NO-UNDO.
DEFINE VARIABLE nv_msgerror7072       AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_exit               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_Acno1              AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Agent              AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_ChkQ               AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_Mdocno1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Mtrty11           AS CHARACTER NO-UNDO. /*M*/
DEFINE VARIABLE nv_Tdocno1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Ttrty11           AS CHARACTER NO-UNDO. /*T*/
DEFINE VARIABLE nv_DuplPolicy        AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_LongCount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_LongTime          AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_SWContractNumber  AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_IChoice           AS LOGICAL NO-UNDO.
DEFINE VARIABLE nv_SplitQno          AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_CountTime         AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_CountLong         AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_CountLeave        AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*Check data Expiry*/
DEFINE VARIABLE nv_expiryrencnt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_expirysigr_p      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nv_expiryprem_t      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nv_expirysclass      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_expirycovcod      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrnRenew          AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_CheckForDB        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE STREAM xmlstream.
/*[COMMENT]*/                    /* */
/*[BLANK]*/                      
DEFINE TEMP-TABLE TFileAttach NO-UNDO
FIELD  FileNameAttach         AS CHARACTER
FIELD  FileBinary             AS BLOB.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------- */
DEFINE NEW SHARED TEMP-TABLE T-DOCNO  NO-UNDO
FIELD  CompNo         AS CHARACTER   /*833*/
FIELD  Branch         AS CHARACTER   /*0*/
FIELD  RunNo          AS INTEGER     /*100*/
FIELD  RcpNoStr       AS INTEGER     /*8330501*/
FIELD  RcpNoEnd       AS INTEGER     /*8330600*/
FIELD  EntDate        AS DATE        /*01/01/14*/
FIELD  RcpFlg         AS CHARACTER   /*T, M*/
FIELD  UseRecp        AS INTEGER     /*¨Ó¹Ç¹àºÍÃì·Õè¶Ù¡ãªéä»*/
FIELD  NextNo         AS INTEGER     /*àºÍÃì¤ÃÑé§µèÍä»*/
FIELD  UseFlag        AS CHARACTER   /*"", FULL*/
/*[COMMENT]*/                    /* */
INDEX T-DOCNO01 IS PRIMARY CompNo  ASCENDING /*833*/
                           RcpFlg  ASCENDING /*T, M*/
                           EntDate ASCENDING /*01/01/14*/
                           UseFlag ASCENDING /*ÇèÒ§ / Full*/
.
DEF VAR nv_covcodtyp1   AS CHAR FORMAT "x(5)"  INIT "". /*Add Kridtiya i. Date. 21/01/2016 */
DEF VAR nv_CompanyCode1 AS CHAR FORMAT "x(10)" INIT "". /*Add Kridtiya i. Date. 14/10/2016 */
DEF VAR nv_timein       AS CHAR FORMAT "x(10)" INIT "".
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
DEF VAR nv_ContractNum AS CHAR INIT "".
DEF VAR nv_docrun2      AS  CHAR.    /* Invoice  */
DEF VAR nv_docrun3      AS  CHAR.    /* Sticker  */
/*[COMMENT]*/                    /*DESCENDING.*/
/*[COMMENT]*/                    /* ------------------------------------------- */
/*[BLANK]*/                      
/*[COMMENT]*/                    /************************************************************************/
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ********************  Preprocessor Definitions  ******************** */
/*[BLANK]*/                      
&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br_QueueRequest
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES IntS7072
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions for BROWSE br_QueueRequest                               */
&Scoped-define FIELDS-IN-QUERY-br_QueueRequest /* */ IntS7072.ProcessStatus IntS7072.TrnFromIntDate IntS7072.TrnFromIntTime IntS7072.CompanyCode IntS7072.ContractNumber /* IntS7072.PolicyNumber */ IntS7072.PolicyTypeCd IntS7072.RateGroup IntS7072.DocumentUID /**/ IntS7072.CMIPolicyTypeCd IntS7072.CMIVehTypeCd IntS7072.CMIBarCodeNumber /* IntS7072.comdat IntS7072.expdat IntS7072.Registration IntS7072.RegisteredProvCd IntS7072.CMIComDate IntS7072.CMIExpDate */ /* END. */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_QueueRequest   
&Scoped-define SELF-NAME br_QueueRequest
&Scoped-define OPEN-QUERY-br_QueueRequest /* OPEN QUERY {&SELF-NAME}     FOR EACH IntS7072 WHERE              IntS7072.SystemRq   = fi_ResponseJob NO-LOCK. */ IF fi_RequestorRq = "ALL" THEN DO:   /* 22/9   FIND FIRST IntS7072 WHERE              IntS7072.SystemRq   = fi_ResponseJob      /* "SPmotor" */          AND IntS7072.MethodCode = STRING(fi_SplitQno) /*22/9*/   NO-LOCK NO-ERROR NO-WAIT.   */   OPEN QUERY {&SELF-NAME}       FOR EACH IntS7072 WHERE                IntS7072.SystemRq   = fi_ResponseJob            AND IntS7072.MethodCode = nv_SplitQno    /*22/9*/    NO-LOCK. END. ELSE DO:   /* 22/9   FIND FIRST IntS7072 WHERE              IntS7072.SystemRq    = fi_ResponseJob     /* "SPmotor" */         AND  IntS7072.CompanyCode = fi_RequestorRq     /* "KK", ~
/*[COMMENT]*/                           à¡ÕÂÃµÔ¹Ò¤Ô¹ */         AND  IntS7072.MethodCode  = STRING(fi_SplitQno) /*22/9*/   NO-LOCK NO-ERROR NO-WAIT.   */   OPEN QUERY {&SELF-NAME}       FOR EACH IntS7072 WHERE                IntS7072.SystemRq    = fi_ResponseJob            AND IntS7072.CompanyCode = fi_RequestorRq /* "KK", ~
       à¡ÕÂÃµÔ¹Ò¤Ô¹ */            AND IntS7072.MethodCode  = nv_SplitQno    /*22/9*/   NO-LOCK.  END.
&Scoped-define TABLES-IN-QUERY-br_QueueRequest IntS7072
&Scoped-define FIRST-TABLE-IN-QUERY-br_QueueRequest IntS7072
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_ResponseJob fi_RequestorRq fi_SplitQno ~
buOK buCANCEL br_QueueRequest fi_RequestorName fi_notfound fi_notfound2 ~
fi_PolicyNumber fi_vehreg fi_Vehicle fi_RegisteredProvinceCode ~
fi_PlateNumber fi_waitcount rd_SystemExt fi_TextRemark RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fi_ResponseJob fi_RequestorRq fi_SplitQno ~
fi_RequestorName fi_notfound fi_notfound2 fi_PolicyNumber fi_vehreg ~
fi_Vehicle fi_RegisteredProvinceCode fi_PlateNumber fi_waitcount ~
rd_SystemExt fi_TextRemark 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Custom List Definitions                                              */
/*[COMMENT]*/                    /* List-1,List-2,List-3,List-4,List-5,List-6                            */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***********************  Control Definitions  ********************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions of the field level widgets                               */
DEFINE BUTTON buCANCEL 
     LABEL "Cancel" 
     SIZE 10 BY 1
     FONT 6.
/*[BLANK]*/                      
DEFINE BUTTON buOK 
     LABEL "OK" 
     SIZE 10 BY 1
     FONT 6.
/*[BLANK]*/                      
DEFINE VARIABLE fi_notfound AS CHARACTER FORMAT "X(75)":U 
      VIEW-AS TEXT 
     SIZE 78 BY .67
     BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_notfound2 AS CHARACTER FORMAT "X(78)":U 
      VIEW-AS TEXT 
     SIZE 78 BY .67
     BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_PlateNumber AS CHARACTER FORMAT "X(15)":U 
     LABEL "·º.Ã¶" 
      VIEW-AS TEXT 
     SIZE 10 BY .62
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_PolicyNumber AS CHARACTER FORMAT "X(40)":U 
     LABEL "Pol." 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_RegisteredProvinceCode AS CHARACTER FORMAT "X(8)":U 
     LABEL "¨Ç.·º.Ã¶" 
      VIEW-AS TEXT 
     SIZE 5 BY .62
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_RequestorName AS CHARACTER FORMAT "X(45)":U 
     LABEL "(ALL=·Ñé§ËÁ´)" 
      VIEW-AS TEXT 
     SIZE 35 BY .71
     BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_RequestorRq AS CHARACTER FORMAT "X(10)":U 
     LABEL "ÃËÑÊºÃÔÉÑ· Broker" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     FGCOLOR 1 FONT 6 NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_ResponseJob AS CHARACTER FORMAT "X(10)":U 
     LABEL "Queue Receive Name" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     FONT 6 NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_SplitQno AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Split Process Job Queue no." 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     FGCOLOR 2 FONT 6 NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_TextRemark AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 93 BY .71
     FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_Vehicle AS CHARACTER FORMAT "X(30)":U 
     LABEL "Model" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_vehreg AS CHARACTER FORMAT "X(15)":U 
     LABEL "¡·.Ã¶" 
      VIEW-AS TEXT 
     SIZE 12 BY .62
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE fi_waitcount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Wait count" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81 NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE rd_SystemExt AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Test", 1,
"PD", 2,
"Oth", 3
     SIZE 20 BY .71
     FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY .19.
/*[BLANK]*/                      
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY .19.
/*[BLANK]*/                      
DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 96.17 BY 1.1
     BGCOLOR 3 .
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_QueueRequest FOR 
      IntS7072 SCROLLING.
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Browse definitions                                                   */
DEFINE BROWSE br_QueueRequest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_QueueRequest C-Win _FREEFORM
  QUERY br_QueueRequest DISPLAY
/*[COMMENT]*/                          /* */
IntS7072.ProcessStatus          COLUMN-LABEL "ST"            FORMAT "xx"
IntS7072.TrnFromIntDate         COLUMN-LABEL "Tran.Date"     FORMAT "99/99/9999"
IntS7072.TrnFromIntTime         COLUMN-LABEL "Tran.Time"     FORMAT "X(08)"      
IntS7072.CompanyCode            COLUMN-LABEL "CompCode"      FORMAT "X(08)"
IntS7072.ContractNumber         COLUMN-LABEL "Contract no."  FORMAT "X(16)"
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    IntS7072.PolicyNumber           COLUMN-LABEL "Policy no."    FORMAT "X(14)" */
IntS7072.PolicyTypeCd           COLUMN-LABEL "PType"         FORMAT "X(4)"
IntS7072.RateGroup              COLUMN-LABEL "Type"          FORMAT "X(5)"
IntS7072.DocumentUID            COLUMN-LABEL "Docno."        FORMAT "X(8)"
/*[COMMENT]*/                    /**/
IntS7072.CMIPolicyTypeCd        COLUMN-LABEL "CmiT"          FORMAT "X(4)"
IntS7072.CMIVehTypeCd           COLUMN-LABEL "CmiV"          FORMAT "X(5)"
IntS7072.CMIBarCodeNumber       COLUMN-LABEL "StickerNo."    FORMAT "X(13)"
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    IntS7072.comdat                 COLUMN-LABEL "Com.Date"      FORMAT "99/99/9999"
/*[COMMENT]*/                    IntS7072.expdat                 COLUMN-LABEL "Exp.Date"      FORMAT "99/99/9999"
/*[COMMENT]*/                    IntS7072.Registration           COLUMN-LABEL "·ÐàºÕÂ¹"       FORMAT "X(11)"
/*[COMMENT]*/                    IntS7072.RegisteredProvCd       COLUMN-LABEL "¨Ç."           FORMAT "X(3)"
/*[COMMENT]*/                    IntS7072.CMIComDate             COLUMN-LABEL "CMIComDt"      FORMAT "99/99/9999"
/*[COMMENT]*/                    IntS7072.CMIExpDate             COLUMN-LABEL "CMIExpDt"      FORMAT "99/99/9999"
/*[COMMENT]*/                    */
/*[COMMENT]*/                    /* END. */
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 5
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Queue ÃÑº¢éÍÁÙÅ V70, V72 Policy à¢éÒÃÐºº - áÅÐµÍº¡ÅÑº" ROW-HEIGHT-CHARS .65.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ************************  Frame Definitions  *********************** */
/*[BLANK]*/                      
DEFINE FRAME DEFAULT-FRAME
     fi_ResponseJob AT ROW 2.38 COL 23.33 COLON-ALIGNED WIDGET-ID 8
     fi_RequestorRq AT ROW 3.24 COL 23.33 COLON-ALIGNED WIDGET-ID 28
     fi_SplitQno AT ROW 4.1 COL 23.33 COLON-ALIGNED WIDGET-ID 178
     buOK AT ROW 2.43 COL 86.5 WIDGET-ID 10
     buCANCEL AT ROW 3.76 COL 86.5 WIDGET-ID 12
     br_QueueRequest AT ROW 8.67 COL 1.5 WIDGET-ID 200
     fi_RequestorName AT ROW 3.33 COL 45.83 COLON-ALIGNED WIDGET-ID 30
     fi_notfound AT ROW 7.24 COL 10.17 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi_notfound2 AT ROW 7.91 COL 10.17 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fi_PolicyNumber AT ROW 6.19 COL 81 COLON-ALIGNED WIDGET-ID 100
     fi_vehreg AT ROW 6.19 COL 37.17 COLON-ALIGNED WIDGET-ID 102
     fi_Vehicle AT ROW 6.19 COL 56.17 COLON-ALIGNED WIDGET-ID 168
     fi_RegisteredProvinceCode AT ROW 6.19 COL 8.5 COLON-ALIGNED WIDGET-ID 170
     fi_PlateNumber AT ROW 6.19 COL 20.17 COLON-ALIGNED WIDGET-ID 172
     fi_waitcount AT ROW 4.1 COL 58 COLON-ALIGNED WIDGET-ID 180
     rd_SystemExt AT ROW 2.38 COL 63 NO-LABEL WIDGET-ID 64
     fi_TextRemark AT ROW 5 COL 3 NO-LABEL WIDGET-ID 182
     "                      Queue ÃÑº¢éÍÁÙÅ V70, V72 ¨Ò¡ DB BUExt à¢éÒ-Êè§ÃÑº¡ÅÑº" VIEW-AS TEXT
          SIZE 65.5 BY .71 AT ROW 1.33 COL 31.5 WIDGET-ID 166
          FGCOLOR 4 FONT 6
     " Execute on Gateway Server" VIEW-AS TEXT
          SIZE 29.17 BY .71 AT ROW 1.33 COL 2.33 WIDGET-ID 164
          FGCOLOR 4 FONT 6
     RECT-6 AT ROW 1.14 COL 1.5 WIDGET-ID 162
     RECT-4 AT ROW 7 COL 4 WIDGET-ID 138
     RECT-5 AT ROW 5.86 COL 4 WIDGET-ID 158
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.17 BY 12.81 WIDGET-ID 100.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* *********************** Procedure Settings ************************ */
/*[BLANK]*/                      
&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/*[COMMENT]*/                    /* Settings for THIS-PROCEDURE
/*[COMMENT]*/                       Type: Window
/*[COMMENT]*/                       Allow: Basic,Browse,DB-Fields,Window,Query
/*[COMMENT]*/                       Other Settings: COMPILE
/*[COMMENT]*/                     */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS
/*[BLANK]*/                      
/*[COMMENT]*/                    /* *************************  Create Window  ************************** */
/*[BLANK]*/                      
&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Safety Insurance Public Company Limited"
         HEIGHT             = 12.62
         WIDTH              = 97
         MAX-HEIGHT         = 45.81
         MAX-WIDTH          = 213.33
         VIRTUAL-HEIGHT     = 45.81
         VIRTUAL-WIDTH      = 213.33
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/*[BLANK]*/                      
&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("WIMAGE/safety.ico":U) THEN
    MESSAGE "Unable to load icon: WIMAGE/safety.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/*[COMMENT]*/                    /* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***********  Runtime Attributes and AppBuilder Settings  *********** */
/*[BLANK]*/                      
&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/*[COMMENT]*/                    /* SETTINGS FOR WINDOW C-Win
/*[COMMENT]*/                      VISIBLE,,RUN-PERSISTENT                                               */
/*[COMMENT]*/                    /* SETTINGS FOR FRAME DEFAULT-FRAME
/*[COMMENT]*/                       FRAME-NAME Custom                                                    */
/*[COMMENT]*/                    /* BROWSE-TAB br_QueueRequest buCANCEL DEFAULT-FRAME */
/*[COMMENT]*/                    /* SETTINGS FOR FILL-IN fi_TextRemark IN FRAME DEFAULT-FRAME
/*[COMMENT]*/                       ALIGN-L                                                              */
/*[COMMENT]*/                    /* SETTINGS FOR RECTANGLE RECT-6 IN FRAME DEFAULT-FRAME
/*[COMMENT]*/                       NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Setting information for Queries and Browse Widgets fields            */
/*[BLANK]*/                      
&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_QueueRequest
/*[COMMENT]*/                    /* Query rebuild information for BROWSE br_QueueRequest
/*[COMMENT]*/                         _START_FREEFORM
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    OPEN QUERY {&SELF-NAME}
/*[COMMENT]*/                        FOR EACH IntS7072 WHERE
/*[COMMENT]*/                                 IntS7072.SystemRq   = fi_ResponseJob
/*[COMMENT]*/                    NO-LOCK.
/*[COMMENT]*/                    */
/*[COMMENT]*/                    IF fi_RequestorRq = "ALL" THEN DO:
/*[COMMENT]*/                      /* 22/9
/*[COMMENT]*/                      FIND FIRST IntS7072 WHERE
/*[COMMENT]*/                                 IntS7072.SystemRq   = fi_ResponseJob      /* "SPmotor" */
/*[COMMENT]*/                             AND IntS7072.MethodCode = STRING(fi_SplitQno) /*22/9*/
/*[COMMENT]*/                      NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                      */
/*[COMMENT]*/                      OPEN QUERY {&SELF-NAME}
/*[COMMENT]*/                          FOR EACH IntS7072 WHERE
/*[COMMENT]*/                                   IntS7072.SystemRq   = fi_ResponseJob
/*[COMMENT]*/                               AND IntS7072.MethodCode = nv_SplitQno    /*22/9*/
/*[BLANK]*/                      
/*[COMMENT]*/                      NO-LOCK.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    ELSE DO:
/*[COMMENT]*/                      /* 22/9
/*[COMMENT]*/                      FIND FIRST IntS7072 WHERE
/*[COMMENT]*/                                 IntS7072.SystemRq    = fi_ResponseJob     /* "SPmotor" */
/*[COMMENT]*/                            AND  IntS7072.CompanyCode = fi_RequestorRq     /* "KK", à¡ÕÂÃµÔ¹Ò¤Ô¹ */
/*[COMMENT]*/                            AND  IntS7072.MethodCode  = STRING(fi_SplitQno) /*22/9*/
/*[COMMENT]*/                      NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                      */
/*[COMMENT]*/                      OPEN QUERY {&SELF-NAME}
/*[COMMENT]*/                          FOR EACH IntS7072 WHERE
/*[COMMENT]*/                                   IntS7072.SystemRq    = fi_ResponseJob
/*[COMMENT]*/                               AND IntS7072.CompanyCode = fi_RequestorRq /* "KK", à¡ÕÂÃµÔ¹Ò¤Ô¹ */
/*[COMMENT]*/                               AND IntS7072.MethodCode  = nv_SplitQno    /*22/9*/
/*[COMMENT]*/                      NO-LOCK.
/*[BLANK]*/                      
/*[COMMENT]*/                    END.
/*[COMMENT]*/                         _END_FREEFORM
/*[COMMENT]*/                         _Query            is OPENED
*/  /* BROWSE br_QueueRequest */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                       
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ************************  Control Triggers  ************************ */
/*[BLANK]*/                      
&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Safety Insurance Public Company Limited */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
/*[COMMENT]*/                      /* This case occurs when the user presses the "Esc" key.
/*[COMMENT]*/                         In a persistently run window, just ignore this.  If we did not, the
/*[COMMENT]*/                         application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Safety Insurance Public Company Limited */
DO:
/*[COMMENT]*/                      /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&Scoped-define SELF-NAME buCANCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buCANCEL C-Win
ON CHOOSE OF buCANCEL IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  Apply "Close" To This-Procedure.  /* »Ô´â»Ãá¡ÃÁ */
  Return No-Apply.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&Scoped-define SELF-NAME buOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buOK C-Win
ON CHOOSE OF buOK IN FRAME DEFAULT-FRAME /* OK */
DO:
/*[COMMENT]*/                      /* */
  ASSIGN
  fi_ResponseJob   = INPUT fi_ResponseJob
  fi_waitcount     = INPUT fi_waitcount
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      fi_RequestorRq   = INPUT fi_RequestorRq*/
  fi_SplitQno      = INPUT fi_SplitQno
  fi_RequestorName = ""
  fi_RequestorRq   = "ALL".
/*[BLANK]*/                      
  IF fi_SplitQno = 0 THEN DO:
    nv_IChoice = NO.
/*[BLANK]*/                      
    MESSAGE "äÁèµéÍ§¡ÒÃÃÐºØ Split Queue"     SKIP(1)
            "â»Ã´Â×¹ÂÑ¹¡ÒÃ´Óà¹Ô¹¡ÒÃ"    SKIP
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
    UPDATE nv_IChoice.
/*[BLANK]*/                      
    IF nv_IChoice = NO THEN DO:
/*[BLANK]*/                      
      APPLY "ENTRY" TO fi_SplitQno.
      RETURN NO-APPLY.
    END.
  END.
/*[BLANK]*/                      
  IF fi_RequestorRq <> "ALL" THEN DO:
/*[BLANK]*/                      
    IF fi_RequestorRq = "" THEN DO:
/*[BLANK]*/                      
      APPLY "ENTRY" TO fi_RequestorRq.
      RETURN NO-APPLY.
    END.
/*[BLANK]*/                      
    FIND FIRST InsurerCode WHERE InsurerCode.InsurerCd = fi_RequestorRq
    NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE InsurerCode THEN DO:
/*[BLANK]*/                      
      MESSAGE "Not found Insurer Code:" fi_RequestorRq SKIP
              "on Table InsurerCode" SKIP (1) 
      VIEW-AS ALERT-BOX.
/*[BLANK]*/                      
      APPLY "ENTRY" TO fi_RequestorRq.
      RETURN NO-APPLY.
    END.
    fi_RequestorName = InsurerCode.InsurerName.
    nv_InsurerCodeRq = fi_RequestorRq.
  END. 
/*[COMMENT]*/                      /* */
/*[BLANK]*/                      
  nv_ConfirmBy = "Auto".
/*[COMMENT]*/                                                  /* Check Confirm Response data AUTO or USERID */
  FIND FIRST  FConfResponse WHERE
              FConfResponse.SystemConfirm = fi_ResponseJob
  NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                                                    /* AUTO or USERID */
  IF AVAILABLE  FConfResponse THEN nv_ConfirmBy = FConfResponse.ConfirmBy.
/*[BLANK]*/                      
  IF fi_SplitQno = 0 THEN nv_SplitQno = "".
                     ELSE nv_SplitQno = STRING(fi_SplitQno).
/*[COMMENT]*/                      /**/
/*[BLANK]*/                      
  DISPLAY fi_RequestorName fi_SplitQno WITH FRAME DEFAULT-FRAME.
  DISABLE buOK    buCANCEL WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
  ASSIGN
    nv_rec_rs  = 0 
    nv_rec_rq  = 0
    nv_SwtFind = "NEXT"
    nv_FindUcf = 0        /*Record find TABLE UserID Confirm*/
    nv_NumLimitRqPerDay   = 0
    nv_ManageTo           = NO
    nv_NumRqPerDay        = 0
    nv_NumRqPerWeek       = 0
    nv_NumRqPerMonth      = 0
    nv_NumRqPerYear       = 0
    nv_ForDate            = TODAY
    nv_Month              = MONTH(TODAY)
    nv_CountLeave         = 0.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ****************************************************************************** */
/*[BLANK]*/                      
  loop_job:
  REPEAT:
/*[BLANK]*/                      
    RUN PD_ClearData.
    RUN PD_DispData.
    RUN PD_DispMess ("").
/*[BLANK]*/                      
    FIND FIRST IntS7072 WHERE 
               IntS7072.SystemRq   = fi_ResponseJob      /* "SPmotor" */
           AND IntS7072.MethodCode = nv_SplitQno
/*[COMMENT]*/                             /*AND IntS7072.MethodCode = STRING(fi_SplitQno) /*22/9*/ */
    NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                        /* --
/*[COMMENT]*/                        IF fi_RequestorRq = "ALL" THEN DO:
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          FIND FIRST IntS7072 WHERE 
/*[COMMENT]*/                                     IntS7072.SystemRq    = fi_ResponseJob     /* "SPmotor" */
/*[COMMENT]*/                                 AND IntS7072.CompanyCode = fi_RequestorRq     /* "KK", à¡ÕÂÃµÔ¹Ò¤Ô¹ */
/*[COMMENT]*/                                 AND IntS7072.MethodCode  = nv_SplitQno
/*[COMMENT]*/                               /*AND IntS7072.MethodCode  = STRING(fi_SplitQno) /*22/9*/ */
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        -- */
    IF NOT AVAILABLE IntS7072 THEN DO:
      HIDE MESSAGE NO-PAUSE.
      ASSIGN nv_process = "" 
      nv_timein  = STRING(TIME,"HH:MM:SS").
/*[BLANK]*/                      
      RUN Pc_CheckDataExt (INPUT nv_ConfirmBy, INPUT-OUTPUT nv_process).
/*[BLANK]*/                      
      RUN PD_DispMess ("Please wait check data db external (DMZ).").
/*[BLANK]*/                      
      IF nv_process = "" THEN DO:
/*[COMMENT]*/                               /*      22.00.00¹.      05.00.00¹.*/
        IF TIME >= 79200 OR TIME <= 18000 THEN DO:
          nv_CountLeave = 0.
/*[COMMENT]*/                              /**/
          PAUSE 1 NO-MESSAGE. /*ÊÓËÃÑº F4 ÍÍ¡ä´é*/
        END.
        ELSE DO:
/*[BLANK]*/                      
/*[COMMENT]*/                              /**/
          IF nv_CountLeave <= 10 THEN DO:
/*[BLANK]*/                        
            nv_CountTime = 0.
            nv_CountLong = 1000000.
            nv_CountLong = 500000.   /*1/4ÇÔ¹Ê·Õ*/
            DO  WHILE nv_CountTime <= nv_CountLong:
              nv_CountTime = nv_CountTime + 1.
            END.
/*[BLANK]*/                        
            nv_CountLeave = nv_CountLeave + 1.
          END.
          ELSE DO:
            nv_CountLeave = 0.
/*[COMMENT]*/                                /**/
            PAUSE 1 NO-MESSAGE. /*ÊÓËÃÑº F4 ÍÍ¡ä´é*/
/*[COMMENT]*/                              /**/
          END. /**/
        END.
      END.
      IF LASTKEY = KEYCODE("F4") THEN LEAVE loop_job.
/*[BLANK]*/                      
      NEXT loop_job.
    END.
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
    IF AVAILABLE IntS7072 THEN nv_rec_rq = RECID(IntS7072).
    ELSE DO:
/*[BLANK]*/                      
      CLOSE QUERY br_QueueRequest.
      nv_rec_rq = 0.
      NEXT loop_job.
    END.
/*[BLANK]*/                      
    CLOSE QUERY br_QueueRequest.
    {&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
    FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
    NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE IntS7072 THEN NEXT loop_job.
/*[BLANK]*/                      
    ASSIGN
    fi_PolicyNumber           = IntS7072.PolicyNumber
    fi_Vehicle                = TRIM(IntS7072.Manufacturer) + " " + IntS7072.Model
    fi_vehreg                 = IntS7072.vehreg
    fi_RegisteredProvinceCode = IntS7072.RegisteredProvinceCode
    fi_PlateNumber            = IntS7072.PlateNumber
/*[BLANK]*/                          
     .
    RUN PD_DispData.
/*[BLANK]*/                      
    nv_count = 0.
    DO  WHILE nv_count <= fi_waitcount:
      nv_count = nv_count + 1.
    END.
/*[BLANK]*/                      
/*[COMMENT]*/                        /* ????????????????????????????????
/*[COMMENT]*/                            PAUSE 10 NO-MESSAGE.
/*[BLANK]*/                      
/*[COMMENT]*/                        PAUSE 1 NO-MESSAGE.
/*[COMMENT]*/                        IF LASTKEY = KEYCODE("F4") THEN LEAVE loop_job.
/*[BLANK]*/                      
/*[COMMENT]*/                        NEXT loop_job.
/*[COMMENT]*/                         */
/*[COMMENT]*/                        /* ------------------------------------------------------------------- */
/*[COMMENT]*/                        /* DELETE DATA FILE IntS7072 AFTER GET DATA FROM FILE IntS7072Result */
/*[BLANK]*/                      
    IF     IntS7072.ProcessStatus = "C"
       OR  IntS7072.ProcessStatus = "E"
       OR  IntS7072.ProcessStatus = "X"
    THEN DO:
/*[BLANK]*/                      
      CLOSE QUERY br_QueueRequest.
/*[BLANK]*/                      
      RUN Pd_DeleteResultX (INPUT-OUTPUT nv_rec_rq).
/*[BLANK]*/                      
/*[BLANK]*/                      
      CLOSE QUERY br_QueueRequest.
      {&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[BLANK]*/                      
      RELEASE EntS7072Result.
      RELEASE EntS7072.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          RELEASE IntS7072Result.
/*[COMMENT]*/                          */
      RELEASE IntS7072.
/*[BLANK]*/                      
      RELEASE IntPol7072Result.
      RELEASE IntPolicy.
/*[BLANK]*/                      
      IF nv_rec_rq <> 0 THEN PAUSE 1 NO-MESSAGE.
/*[BLANK]*/                      
      IF LASTKEY = KEYCODE("F4") THEN LEAVE loop_job.
      NEXT loop_job.
    END.
/*[COMMENT]*/                        /* ------------------------------- */
/*[BLANK]*/                      
/*[BLANK]*/                      
    FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
    NO-LOCK NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[COMMENT]*/                        /* --------------------------------------------------- */
    IF     IntS7072.ProcessStatus = "C"
       OR  IntS7072.ProcessStatus = "E"
       OR  IntS7072.ProcessStatus = "X"  THEN NEXT loop_job.
/*[BLANK]*/                      
    IF      IntS7072.ProcessStatus = "O" 
    THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          /* ÃÐºØãËéÁÕ¡ÒÃµÃÇ¨ÊÀÒ¾Ã¶ */
/*[BLANK]*/                      
      ASSIGN
      nv_PolicyV70 = ""
      nv_DocnoV70  = ""
/*[COMMENT]*/                          /**/       
      nv_PolicyV72 = ""
      nv_DocnoV72  = ""
      nv_msgerror  = ""
      nv_CompanyCode1 = "".  /*Add Kridtiya i. date. 14/10/2016 */
/*[COMMENT]*/                          /*IF (IntS7072.CompanyCode  = "834") AND (IntS7072.SERVICE_ID = "" ) THEN DO:  /*Add Kridtiya i. date. 14/10/2016 */
/*[COMMENT]*/                              IF IntS7072.PolicyTypeCd <> "" THEN nv_CompanyCode1 = "834Old".   
/*[COMMENT]*/                              ELSE nv_CompanyCode1 = "834".   
/*[COMMENT]*/                          END.                                         /*Add Kridtiya i. date. 14/10/2016 */
      ELSE nv_CompanyCode1 = IntS7072.CompanyCode.                                 /*Add Kridtiya i. date. 14/10/2016 */*/
          nv_CompanyCode1 = IntS7072.CompanyCode.    /*Add Kridtiya i. date. 30/06/2017 */
/*[BLANK]*/                      
      IF IntS7072.ConfirmBy <> "AUTO" THEN DO: 
/*[BLANK]*/                              
/*[COMMENT]*/                            /*QUOTATION NUMBER ÊÓËÃÑºµÃÇ¨ÊÀÒ¾Ã¶*/
/*[BLANK]*/                      
        IF IntS7072.PolicyTypeCd <> "" THEN DO:
          nv_ChkQ = "".
/*[BLANK]*/                      
          FIND FIRST FUtilSetUp WHERE
                     FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
                 AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
                 AND FUtilSetUp.KeyUtilGrp2  = "Chk"
                 AND FUtilSetUp.KeyUtilGrp3  = "Q"
                 AND FUtilSetUp.KeyUtilGrp4  = "Policy"
                 AND FUtilSetUp.KeyUtilGrp5  = "ALL"         /*833*/
                 AND FUtilSetUp.EffDate     <= TODAY
          NO-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[BLANK]*/                      
          ELSE DO:
/*[BLANK]*/                      
            FIND FIRST FUtilSetUp WHERE
                       FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
                   AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
                   AND FUtilSetUp.KeyUtilGrp2  = "Chk"
                   AND FUtilSetUp.KeyUtilGrp3  = "Q"
                   AND FUtilSetUp.KeyUtilGrp4  = "Policy"
                   AND FUtilSetUp.KeyUtilGrp5  = IntS7072.CompanyCode
                   AND FUtilSetUp.EffDate     <= TODAY
            NO-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
          END.
/*[BLANK]*/                      
          IF nv_ChkQ = "YES" THEN DO:
/*[BLANK]*/                                   
/*[COMMENT]*/                                /*RUN WSP/WSPRunP1WS.P  (INPUT  IntS7072.CompanyCode*/  /*Add Kridtiya i. date. 14/10/2016 */  
            RUN WSP/WSPRunP1WS.P  (INPUT  nv_CompanyCode1           /*Add Kridtiya i. date. 14/10/2016 */  
                                  ,INPUT  IntS7072.BranchCd
                                  ,INPUT  "V70"  /*¢éÒ§ã¹ NEW=Q*/
                                  ,INPUT  IntS7072.PreviousPolicyNumber
                                  ,INPUT  IntS7072.PolicyStatus
                                  ,INPUT  IntS7072.PolicyTypeCd
                                  ,INPUT  IntS7072.SumInsureAmt
/*[COMMENT]*/                                                       /**/
                                  ,OUTPUT nv_PolicyV70 /*nv_Policy1*/
                                  ,OUTPUT nv_msgerror).
          END.
          ELSE DO:
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
            OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
            PUT "1. v70 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
            OUTPUT CLOSE.
/*[COMMENT]*/                                /*RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode*//*Add Kridtiya i. date. 14/10/2016 */  
            RUN WSP/WSPRunP2WS.P  (INPUT  nv_CompanyCode1         /*Add Kridtiya i. date. 14/10/2016 */  
                                  ,INPUT  IntS7072.BranchCd
                                  ,INPUT  "V70"
                                  ,OUTPUT nv_PolicyV70 /*nv_PolicyV70*/
                                  ,OUTPUT nv_msgerror).
          END.
/*[BLANK]*/                      
          IF TRIM(nv_msgerror) <> "" OR TRIM(nv_PolicyV70) = "" THEN DO:
/*[BLANK]*/                      
            IF nv_msgerror = "" THEN
               nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Quotation Policy ¢Í§ Company code: "
                           + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                                  
            OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
            PUT "1. " nv_msgerror FORMAT "X(250)" SKIP.
            OUTPUT CLOSE.
/*[BLANK]*/                      
            RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
            RUN PD_DispData.
            RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                PAUSE 5.*/
/*[BLANK]*/                      
            NEXT loop_job.
          END.
        END.
/*[BLANK]*/                      
        IF IntS7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                              /*
/*[COMMENT]*/                              RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
          OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
          PUT "2. v72 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
          OUTPUT CLOSE.
/*[BLANK]*/                      
          RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
                                ,INPUT  IntS7072.BranchCd
                                ,INPUT  "V72"        /*QV72*/
                                ,OUTPUT nv_PolicyV72
                                ,OUTPUT nv_msgerror).
/*[BLANK]*/                      
          IF TRIM(nv_msgerror) <> "" OR TRIM(nv_PolicyV72) = "" THEN DO:
/*[BLANK]*/                      
            IF nv_msgerror = "" THEN
               nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Quotation Policy ¾Ãº. ¢Í§ Company code: "
                           + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                      
            OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
            PUT "2. " nv_msgerror FORMAT "X(250)" SKIP.
            OUTPUT CLOSE.
/*[BLANK]*/                      
            RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
            RUN PD_DispData.
            RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                PAUSE 5. */
/*[BLANK]*/                      
            NEXT loop_job.
          END.
        END.
/*[BLANK]*/                      
      END.
/*[COMMENT]*/                          /* ÊÔé¹ÊØ´  ÃÐºØãËéÁÕ¡ÒÃµÃÇ¨ÊÀÒ¾Ã¶ */
/*[COMMENT]*/                          /* ------------------------------- */
      ELSE DO:
/*[BLANK]*/                             
        IF IntS7072.ConfirmBy = "AUTO" THEN DO:
/*[BLANK]*/                      
          IF IntS7072.PolicyTypeCd <> "" THEN DO:
/*[BLANK]*/                      
            nv_ChkQ = "".
/*[BLANK]*/                      
            FIND FIRST FUtilSetUp WHERE
                       FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
                   AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
                   AND FUtilSetUp.KeyUtilGrp2  = "Chk"
                   AND FUtilSetUp.KeyUtilGrp3  = "Q"
                   AND FUtilSetUp.KeyUtilGrp4  = "Policy"
                   AND FUtilSetUp.KeyUtilGrp5  = "ALL"         /*833*/
                   AND FUtilSetUp.EffDate     <= TODAY
            NO-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[BLANK]*/                      
            ELSE DO:
/*[BLANK]*/                      
              FIND FIRST FUtilSetUp WHERE
                         FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
                     AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
                     AND FUtilSetUp.KeyUtilGrp2  = "Chk"
                     AND FUtilSetUp.KeyUtilGrp3  = "Q"
                     AND FUtilSetUp.KeyUtilGrp4  = "Policy"
                     AND FUtilSetUp.KeyUtilGrp5  = IntS7072.CompanyCode
                     AND FUtilSetUp.EffDate     <= TODAY
              NO-LOCK NO-ERROR NO-WAIT.
              IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
            END.
/*[BLANK]*/                      
            IF nv_ChkQ = "YES" THEN DO:
/*[COMMENT]*/                                  /*RUN WSP/WSPRunP1WS.P  (INPUT  IntS7072.CompanyCode */ /*Add Kridtiya i. date. 14/10/2016 */  
              RUN WSP/WSPRunP1WS.P  (INPUT  nv_CompanyCode1           /*Add Kridtiya i. date. 14/10/2016 */  
                                    ,INPUT  IntS7072.BranchCd
                                    ,INPUT  "V70"  /*¢éÒ§ã¹ NEW=Q*/
                                    ,INPUT  IntS7072.PreviousPolicyNumber
                                    ,INPUT  IntS7072.PolicyStatus
                                    ,INPUT  IntS7072.PolicyTypeCd
                                    ,INPUT  IntS7072.SumInsureAmt
/*[COMMENT]*/                                                         /**/
                                    ,OUTPUT nv_PolicyV70 /*nv_Policy1*/
                                    ,OUTPUT nv_msgerror).
            END.
            ELSE DO:
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
              OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
              PUT "3. v70 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
              OUTPUT CLOSE.
/*[BLANK]*/                                    
              RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
                                    ,INPUT  IntS7072.BranchCd
                                    ,INPUT  "V70"
                                    ,OUTPUT nv_PolicyV70
                                    ,OUTPUT nv_msgerror).
            END.
/*[BLANK]*/                      
            IF nv_msgerror <> "" THEN DO:
/*[BLANK]*/                      
              IF nv_msgerror = "" THEN
                 nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Policy ¢Í§ Company code: "
                             + IntS7072.CompanyCode + IntS7072.Username.
              OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
              PUT "3. " nv_msgerror FORMAT "X(250)" SKIP.
              OUTPUT CLOSE.
/*[BLANK]*/                      
              RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
              RUN PD_DispData.
              RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  PAUSE 5. */
/*[BLANK]*/                      
              NEXT loop_job.
            END.          
          END.
/*[BLANK]*/                      
          IF IntS7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
            OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
            PUT "4. v72 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
            OUTPUT CLOSE.
/*[BLANK]*/                      
            RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
                                  ,INPUT  IntS7072.BranchCd
                                  ,INPUT  "V72"
                                  ,OUTPUT nv_PolicyV72
                                  ,OUTPUT nv_msgerror).
/*[BLANK]*/                      
            IF nv_msgerror <> "" THEN DO:
/*[BLANK]*/                      
              IF nv_msgerror = "" THEN
                 nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Policy ¾Ãº. ¢Í§ Company code: "
                             + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                      
              OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
              PUT "4. " nv_msgerror FORMAT "X(250)" SKIP.
              OUTPUT CLOSE.
/*[BLANK]*/                      
              RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
              RUN PD_DispData.
              RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  PAUSE 5. */
/*[BLANK]*/                      
              NEXT loop_job.
            END.
          END.
/*[BLANK]*/                      
        END.
      END.
/*[BLANK]*/                      
      RUN Pc_SAVEProduction (nv_rec_rq
                            ,nv_PolicyV70 /*,nv_DocnoV70*/
                            ,nv_PolicyV72 /*,nv_DocnoV72*/
                            ).
      OUTPUT TO LogTimeInOut7072.TXT APPEND.
      PUT "Log Time:" 
          TODAY FORMAT "99/99/9999" " Time: "  STRING(TIME,"HH:MM:SS") "-" nv_timein    
          " Company:"  nv_CompanyCode1 "Policy:" nv_PolicyV70 nv_PolicyV72 FORMAT "X(150)" SKIP.
      OUTPUT CLOSE.
/*[BLANK]*/                      
      RELEASE EntS7072Result.
      RELEASE EntS7072.
/*[BLANK]*/                      
      RELEASE IntPol7072Result.
      RELEASE IntPolicy.
    END.
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
/*[BLANK]*/                      
  END.   /* l o o p _ j o b : */
/*[COMMENT]*/                      /* ****************************************************************************** */
/*[BLANK]*/                      
  RELEASE EntS7072Result.
  RELEASE EntS7072.
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RELEASE IntS7072Result.
/*[COMMENT]*/                      */
  RELEASE IntS7072.
/*[BLANK]*/                      
  RELEASE IntPol7072Result.
  RELEASE IntPolicy.
/*[BLANK]*/                      
  CLOSE QUERY br_QueueRequest.
  fi_notfound  = "".
  fi_notfound2 = "".
  DISPLAY fi_notfound fi_notfound2 WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
  ENABLE  buOK   buCANCEL  WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
  APPLY "ENTRY" TO fi_ResponseJob.
  RETURN NO-APPLY.
END. /* DO: */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&Scoped-define SELF-NAME fi_RequestorRq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_RequestorRq C-Win
ON LEAVE OF fi_RequestorRq IN FRAME DEFAULT-FRAME /* ÃËÑÊºÃÔÉÑ· Broker */
DO:
  fi_RequestorRq = INPUT fi_RequestorRq.
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      FIND FIRST InsurerCode WHERE InsurerCode.InsurerCd = fi_ProviderRq
/*[COMMENT]*/                      NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                      IF NOT AVAILABLE InsurerCode THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        MESSAGE "Not found Insurer Code:" fi_ProviderRq SKIP
/*[COMMENT]*/                               " on Table InsurerCode" SKIP (1) 
/*[COMMENT]*/                        VIEW-AS ALERT-BOX.
/*[BLANK]*/                      
/*[COMMENT]*/                        APPLY "ENTRY" TO fi_ProviderRq.
/*[COMMENT]*/                        RETURN NO-APPLY.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      */
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
&Scoped-define BROWSE-NAME br_QueueRequest
&UNDEFINE SELF-NAME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***************************  Main Block  *************************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* The CLOSE event can be used from inside or outside the procedure to  */
/*[COMMENT]*/                    /* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Now enable the interface and wait for the exit condition.            */
/*[COMMENT]*/                    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
/*[COMMENT]*/                    /* --- */
CLEAR  ALL     NO-PAUSE.
STATUS INPUT   OFF.
HIDE   MESSAGE NO-PAUSE.
/*[COMMENT]*/                    /* ------------------------------------------------------------------ */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* --- */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*[BLANK]*/                       
/*[COMMENT]*/                      /********************  T I T L E   F O R  C - W I N  ****************/
/*[BLANK]*/                        
  DEF  VAR  gv_prgid   AS   CHAR  FORMAT "X(8)"  NO-UNDO.
  DEF  VAR  gv_prog    AS   CHAR  FORMAT "X(40)" NO-UNDO.
  gv_prgid = "WRSBQ7072".
  gv_prog  = "Queue Get & Send data Policy Motor".
/*[BLANK]*/                      
  RUN  WSU\WSUHDExt ({&WINDOW-NAME}:handle,gv_prgid,gv_prog).
/*[BLANK]*/                      
  RUN  WUT\WUTWICEN (C-WIN:handle).  
/*[COMMENT]*/                      /*********************************************************************/
/*[COMMENT]*/                      /* */
  SESSION:DATA-ENTRY-RETURN = YES.      /* ÃÑº¤èÒ»ØèÁ ENTER */
/*[BLANK]*/                        
/*[COMMENT]*/                      /* ãÊè¤èÒµÑÇá»ÃáÅÐáÊ´§¤èÒ */
  fi_RequestorRq = "ALL".
  fi_ResponseJob = "SPmotor".
  fi_SplitQno = 7.
  rd_SystemExt = 2.
  fi_TextRemark = "M82 1107 1018 834 1470 1146 2117 1554 ".
  DISPLAY fi_RequestorRq fi_ResponseJob WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* */
/*[BLANK]*/                      
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* **********************  Internal Procedures  *********************** */
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     DISABLE the User Interface
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       Here we clean-up the user-interface by deleting
/*[COMMENT]*/                                   dynamic widgets we have created and/or hide 
/*[COMMENT]*/                                   frames.  This procedure is usually called when
/*[COMMENT]*/                                   we are ready to "clean-up" after running.
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                      /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     ENABLE the User Interface
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       Here we display/view/enable the widgets in the
/*[COMMENT]*/                                   user-interface.  In addition, OPEN all queries
/*[COMMENT]*/                                   associated with each FRAME and BROWSE.
/*[COMMENT]*/                                   These statements here are based on the "Other 
/*[COMMENT]*/                                   Settings" section of the widget Property Sheets.
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
  DISPLAY fi_ResponseJob fi_RequestorRq fi_SplitQno fi_RequestorName fi_notfound 
          fi_notfound2 fi_PolicyNumber fi_vehreg fi_Vehicle 
          fi_RegisteredProvinceCode fi_PlateNumber fi_waitcount rd_SystemExt 
          fi_TextRemark 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi_ResponseJob fi_RequestorRq fi_SplitQno buOK buCANCEL 
         br_QueueRequest fi_RequestorName fi_notfound fi_notfound2 
         fi_PolicyNumber fi_vehreg fi_Vehicle fi_RegisteredProvinceCode 
         fi_PlateNumber fi_waitcount rd_SystemExt fi_TextRemark RECT-4 RECT-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_CheckDataExt C-Win 
PROCEDURE Pc_CheckDataExt :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       (INPUT nv_ConfirmBy, INPUT-OUTPUT nv_process).
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT        PARAMETER nv_ConfirmBy AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_process   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_RECIDEntS7072 AS RECID     NO-UNDO.
DEFINE VARIABLE nv_msgerrordupl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_user          AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_errort        AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PolicyTypeCd  AS CHARACTER INITIAL "" NO-UNDO.
/*[COMMENT]*/                    /**/
ASSIGN
nv_RECIDEntS7072 = 0
nv_process       = ""
nv_msgerrordupl  = ""
nv_Acno1   = ""
nv_Agent   = ""
nv_Mdocno1 = ""
nv_Mtrty11 = ""
nv_Tdocno1 = ""
nv_Ttrty11 = ""
nv_DuplPolicy = "".
  FIND FIRST EntS7072  WHERE
             EntS7072.SystemRq      = fi_ResponseJob /* "SPmotor" */
         AND EntS7072.ProcessStatus = ""             /* ""=New , "O"=Get data*/
         AND EntS7072.MethodCode    = nv_SplitQno    /*22/9*/
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE EntS7072 THEN DO:
    nv_LongCount = 0.
    nv_RECIDEntS7072 = RECID(EntS7072).
    IF EntS7072.CompanyCode <> "1146" THEN RUN Pc_SAVECKAGT (INPUT TRIM(EntS7072.CompanyCode)
                                                            ,INPUT-OUTPUT nv_msgerrordupl).
    IF nv_msgerrordupl  = "" THEN RUN Pc_SAVECKGrp (INPUT nv_RECIDEntS7072
                                                   ,INPUT-OUTPUT nv_msgerrordupl ).
/*[COMMENT]*/                        /*Add Check blacklist by Kridtiya i. Date. 27/01/2018*/
    IF nv_msgerrordupl  = "" THEN RUN Pc_SAVEFMRate (INPUT nv_RECIDEntS7072   
                                                     ,INPUT-OUTPUT nv_msgerrordupl ).
    IF ( nv_msgerrordupl  = "" ) THEN DO:   /*AND ( EntS7072.PolicyStatus = "N" ) THEN DO:*/
        IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO:  /* 70 */ 
          RUN wrs/WRSGUCKLTWS           /* WRSGUCKLT.P  */      
              (INPUT  trim(trim(EntS7072.Registration) + " " + trim(EntS7072.RegisteredProvCd)) /*nn_vehreglist*/  
              ,INPUT  trim(EntS7072.InsuredName)                                                /*nn_namelist */
              ,INPUT  trim(EntS7072.InsuredSurname) 
              ,INPUT  trim(EntS7072.ChassisSerialNumber)                                        /*nv_chanolist*/
              ,INPUT  trim(EntS7072.InsuredUniqueID)                                            /*nv_idnolist*/
              ,OUTPUT nv_msgerrordupl).
        END.
    END.
/*[COMMENT]*/                        /*Add Check blacklist by Kridtiya i. Date. 27/01/2018*/
    IF nv_msgerrordupl  = "" THEN DO:
/*[COMMENT]*/                             /*ISUZU*/
      IF     EntS7072.PolicyStatus          = "R"
         AND EntS7072.PreviousPolicyNumber <> ""
         AND EntS7072.PolicyTypeCd         <> ""
/*[COMMENT]*/                           /*AND EntS7072.CompanyCode           = "476" /*ISUZU*/ *//*comment by Kridtiya i. 17/06/2017*/
/*[COMMENT]*/                          /* AND SUBSTR(EntS7072.PolicyNumber,1,2) = "74" */
      THEN DO:
        ASSIGN 
        nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
        nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew     = "".
        IF NOT CONNECTED ("expiry") THEN DO:
          RUN WRS/WRSGU1DB.P
               (""         /*Userid*/
               ,""         /*Password*/
               ,"expiry"). /*Database name*/
        END.
        IF CONNECTED ("expiry") THEN DO:   /*Check ·Ø¹ /àºÕéÂ /covcod ·Õè expiry µÃ§¡Ñ¹ËÃ×ÍäÁè*/
            RUN WRS/WRSGUCKE.P ("EXT"           /*DB BUExt*/
                               ,RECID(EntS7072)
/*[COMMENT]*/                                                    /**/
                               ,INPUT-OUTPUT nv_expiryrencnt /*expiry.uwm100.rencnt*/
                               ,INPUT-OUTPUT nv_expirysigr_p
                               ,INPUT-OUTPUT nv_expiryprem_t
                               ,INPUT-OUTPUT nv_expirysclass
                               ,INPUT-OUTPUT nv_expirycovcod
                               ,INPUT-OUTPUT nv_PrnRenew
                               ,INPUT-OUTPUT nv_msgerrordupl).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN PD_PUTError ("EX1"). */
            IF nv_PrnRenew = "YES" THEN nv_msgerrordupl = "". /*¢éÍÁÙÅ expiry à·èÒ¡Ñ¹*/
        END.
      END. /*IF EntS7072.PolicyStatus = "R"*/
    END. /*IF nv_msgerrordupl  = "" THEN DO:*/
    IF nv_msgerrordupl  = "" THEN DO:
      IF (EntS7072.CompanyCode = "570" OR EntS7072.CompanyCode = "2382") THEN DO:
          IF trim(EntS7072.PolicyTypeCd) <> "" THEN DO:  /*»ÃÐàÀ· 1 2 3*/
              FIND LAST IntPol7072 USE-INDEX IntPol707210 WHERE
                  IntPol7072.CompanyCode               = EntS7072.CompanyCode AND 
                 ( substr(IntPol7072.PolicyTypeCd,1,1) = "1"   OR 
                  substr(IntPol7072.PolicyTypeCd,1,1)  = "2"   OR
                  substr(IntPol7072.PolicyTypeCd,1,1)  = "3" ) AND
                  IntPol7072.ChassisSerialNumber       = EntS7072.ChassisSerialNumber  
                  NO-LOCK NO-ERROR NO-WAIT.
              IF AVAILABLE IntPol7072 THEN DO:
                  IF  EntS7072.EffectiveDt  < IntPol7072.ExpirationDt THEN DO:
                      IF INDEX(IntPol7072.ContractNumber,"CA") <> 0 THEN
                      nv_msgerrordupl = "¾ºàÅ¢·ÕèµÑÇ¶Ñ§«éÓ : " + EntS7072.ChassisSerialNumber + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
                  END.
              END.
          END.
          ELSE DO: /*compulsary*/
              FIND LAST IntPol7072 USE-INDEX IntPol707211 WHERE
                  IntPol7072.CompanyCode         = EntS7072.CompanyCode AND 
                  IntPol7072.CMIPolicyTypeCd     = TRIM(EntS7072.CMIPolicyTypeCd) AND
                  IntPol7072.ChassisSerialNumber = EntS7072.ChassisSerialNumber   NO-LOCK NO-ERROR NO-WAIT.
              IF AVAILABLE IntPol7072 THEN DO:
                  IF  EntS7072.CMIEffectiveDt  < IntPol7072.CMIExpirationDt THEN DO:
                      IF INDEX(IntPol7072.ContractNumber,"CA") <> 0 THEN 
                      nv_msgerrordupl = "¾ºàÅ¢·ÕèµÑÇ¶Ñ§«éÓ : " + EntS7072.ChassisSerialNumber + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
                  END.
              END.
          END.
      END. /*end. 570 */
      IF  EntS7072.PolicyStatus = "N"  THEN DO:
          DEF VAR nv_titleNM AS CHAR FORMAT "x(150)".
          ASSIGN nv_titleNM = ""
              nv_titleNM = EntS7072.InsuredTitle + EntS7072.InsuredName +  EntS7072.InsuredSurname.
          IF EntS7072.PolicyTypeCd = "2.1" OR EntS7072.PolicyTypeCd = "2.2"  OR
              EntS7072.PolicyTypeCd = "3.1" OR EntS7072.PolicyTypeCd = "3.2" OR EntS7072.PolicyTypeCd = "3" THEN DO:
              IF  R-INDEX(TRIM(nv_titleNM),"¨¡.")             <> 0  OR R-INDEX(TRIM(nv_titleNM),"¨Ó¡Ñ´")           <> 0  OR  
                  R-INDEX(TRIM(nv_titleNM),"(ÁËÒª¹)")         <> 0  OR R-INDEX(TRIM(nv_titleNM),"INC.")            <> 0  OR 
                  R-INDEX(TRIM(nv_titleNM),"CO.")             <> 0  OR R-INDEX(TRIM(nv_titleNM),"LTD.")            <> 0  OR 
                  R-INDEX(TRIM(nv_titleNM),"LIMITED")         <> 0  OR INDEX(TRIM(nv_titleNM),"ºÃÔÉÑ·")            <> 0  OR 
                  INDEX(TRIM(nv_titleNM),"º.")                <> 0  OR INDEX(TRIM(nv_titleNM),"º¨¡.")              <> 0  OR 
                  INDEX(TRIM(nv_titleNM),"Ë¨¡.")              <> 0  OR INDEX(TRIM(nv_titleNM),"ËÊ¹.")              <> 0  OR 
                  INDEX(TRIM(nv_titleNM),"ºÃÃÉÑ·")            <> 0  OR INDEX(TRIM(nv_titleNM),"ÁÙÅ¹Ô¸Ô")           <> 0  OR 
                  INDEX(TRIM(nv_titleNM),"ËéÒ§")              <> 0  OR INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹")      <> 0  OR 
                  INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹¨Ó¡Ñ´") <> 0  OR INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹¨Ó¡")   <> 0  OR  
                  INDEX(TRIM(nv_titleNM),"áÅÐ/ËÃ×Í")          <> 0  THEN 
                  nv_msgerrordupl = "¾º§Ò¹¹ÔµÔºØ¤Å: " + EntS7072.ContractNumber + "ª×èÍ : " + 
                  EntS7072.InsuredTitle + " " + EntS7072.InsuredName + " " +  EntS7072.InsuredSurname.   
          END.
      END.
/*[COMMENT]*/                          /* Add Kridtiya i. Date.11/01/2018 à¾ÔèÁ ¡ÒÃàªç¤ ¹ÔµÔºØ¤¤Å*/
      IF EntS7072.MsgStatusCd <> "TEST" THEN DO:
        IF EntS7072.CompanyCode = "M82" THEN DO:
            DEF VAR nv_contactM82 AS CHAR INIT "".
            ASSIGN nv_contactM82 =  substr(EntS7072.ContractNumber,1,11).
/*[COMMENT]*/                                /*FIND FIRST IntPol7072 USE-INDEX IntPol707205 WHERE*/
            RUN Pc_CheckMsg (INPUT ("in check contract" + EntS7072.ContractNumber )).
            FIND LAST IntPol7072  USE-INDEX  IntPol707212 WHERE
                IntPol7072.CompanyCode    = EntS7072.CompanyCode AND   
                IntPol7072.SERVICE_RUN_NO = nv_contactM82    NO-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE IntPol7072 THEN DO:
                IF INDEX(IntPol7072.ContractNumber,"CA") = 0 THEN DO:
                  IF IntPol7072.PolicyNumber <> "" THEN 
                    nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                    + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
                  ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
                    nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                    + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
                END.
            END.
            RUN Pc_CheckMsg (INPUT "out check contract" ).
        END.
        ELSE DO:
          FIND FIRST IntPol7072 USE-INDEX IntPol707205 WHERE
            IntPol7072.ContractNumber = EntS7072.ContractNumber AND
            IntPol7072.CompanyCode    = EntS7072.CompanyCode
            NO-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE IntPol7072 THEN DO:
              IF IntPol7072.CompanyCode = "1018" THEN DO:
                  ASSIGN nv_RecIntPol7072 = RECID(IntPol7072).
                  IF  IntPol7072.AttachFile1  = ? THEN DO:
                      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072 NO-ERROR.
                      IF AVAILABLE IntPol7072 THEN  
                          ASSIGN IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA".
                      RELEASE IntPol7072.
                  END.
                  ELSE DO:
                      IF IntPol7072.PolicyNumber <> "" THEN 
                          nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                          + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
                      ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
                          nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                          + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
                  END.
              END.
              ELSE DO:
                  IF IntPol7072.PolicyNumber <> "" THEN 
                      nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                      + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
                  ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
                      nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                      + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
              END.  /* case no 1018*/
          END.
        END. /*ELSE DO:*/
        IF nv_msgerrordupl  = "" THEN DO:
          FIND FIRST IntS7072 WHERE
                     IntS7072.CompanyCode    = EntS7072.CompanyCode
               AND   IntS7072.ContractNumber = EntS7072.ContractNumber
          NO-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE IntS7072 THEN DO:
            IF IntS7072.PolicyNumber <> "" THEN 
              nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                              + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntS7072.PolicyNumber.
            ELSE IF IntS7072.CMIPolicyNumber <> "" THEN 
              nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
                              + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntS7072.CMIPolicyNumber.
            ELSE nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber + " «éÓã¹ÃÐºº".
          END.
          RUN Pc_CheckMsg (INPUT "in check contract/IntS7072" ).
        END.
      END. /*IF EntS7072.MsgStatusCd <> "TEST" THEN DO:*/
    END. /*IF nv_msgerrordupl = "" THEN DO:*/
    IF nv_msgerrordupl = "" THEN DO:
      IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO: /* 3.1, 3.2, 3*/
/*[COMMENT]*/                                                               /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
        RUN WSP/WSPMCpny.P (EntS7072.CompanyCode
                           ,EntS7072.BranchCd
                           ,"V70"
                           ,OUTPUT nv_BrokerCompany
                           ,OUTPUT nv_BrokerBranch 
                           ,OUTPUT nv_Acno1
                           ,OUTPUT nv_Agent
                           ,OUTPUT nv_errort).
        IF EntS7072.DocumentUID <> ""  THEN DO:
          ASSIGN
          nv_Mdocno1  = EntS7072.DocumentUID
          nv_Mtrty11  = "M".
        END.
      END. /*IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO:*/
      IF nv_msgerrordupl = "" THEN DO:
        IF TRIM(EntS7072.CMIPolicyTypeCd) <> "" AND EntS7072.CMIVehTypeCd <> "" THEN DO: /* ¾Ãº. */
          RUN WSP/WSPMCpny.P (EntS7072.CompanyCode  /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
                             ,EntS7072.BranchCd
                             ,"V72"
                             ,OUTPUT nv_BrokerCompany
                             ,OUTPUT nv_BrokerBranch 
                             ,OUTPUT nv_Acno1
                             ,OUTPUT nv_Agent
                             ,OUTPUT nv_errort).
          IF EntS7072.CMIDocumentUID <> "" THEN DO:
            ASSIGN nv_Tdocno1  = EntS7072.CMIDocumentUID
            nv_Ttrty11  = "T".
          END.
          IF nv_msgerrordupl = "" THEN DO:
            nv_PolicyTypeCd = EntS7072.CMIPolicyTypeCd.
            nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,CHR(13),"").
            nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd," ","").
            nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,".","").
            nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,"-","").
            nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,"+","").
            FOR EACH msgcode WHERE MsgCode.CompNo = EntS7072.CompanyCode
                 AND MsgCode.MsgNo  = "GrpClass"
            NO-LOCK:
              IF msgcode.Branch = nv_PolicyTypeCd AND MsgCode.MsgDesc = "NOTSALE" THEN
                nv_msgerrordupl = "äÁè¾ºÃËÑÊ¾Ãº. " + EntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ".
            END.
          END.
        END.
      END.
    END.
    RUN Pc_CheckMsg (INPUT "in check contract/PD_ChkDataExt1" ).
    IF nv_msgerrordupl = "" THEN DO:
      FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
      NO-ERROR NO-WAIT.
      IF AVAILABLE EntS7072 THEN DO:
        RUN PD_ChkDataExt1 (INPUT nv_Mdocno1
                           ,INPUT nv_Tdocno1).
        nv_process       = "Data".
      END.
    END.
  END. /* FIND FIRST  EntS7072  WHERE*/
IF nv_msgerrordupl = "" THEN DO:
  IF nv_RECIDEntS7072 <> 0 AND nv_RECIDEntS7072 <> ? THEN DO:
    OUTPUT STREAM xmlstream TO PUT_EntS7072.TXT  APPEND .
    FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
    NO-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE EntS7072 THEN DO:
      PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
      PUT STREAM xmlstream "MethodCode SPLIT   |" EntS7072.MethodCode       SKIP.  
      PUT STREAM xmlstream "1  Username        |" EntS7072.Username         SKIP.
      PUT STREAM xmlstream "2  Password        |" EntS7072.Password         SKIP.
      PUT STREAM xmlstream "3  CompanyCode     |" EntS7072.CompanyCode      SKIP.
      PUT STREAM xmlstream "3  BranchCd        |" EntS7072.BranchCd         SKIP.
      PUT STREAM xmlstream "7  ContractNumber  |" EntS7072.ContractNumber   FORMAT "X(30)" SKIP.
      PUT STREAM xmlstream "4  EnsurerId       |" EntS7072.InsurerId        SKIP.
      PUT STREAM xmlstream "4  InsuranceCd     |" EntS7072.InsuranceCd      SKIP.
      PUT STREAM xmlstream "5  PolicyNumber    |" EntS7072.PolicyNumber     SKIP.
      PUT STREAM xmlstream "7  nv_DocnoV70     |" EntS7072.DocumentUID      FORMAT "X(20)" SKIP.
      PUT STREAM xmlstream "24 InsuredName     |" EntS7072.InsuredName      SKIP.
      PUT STREAM xmlstream "25 InsuredSurname  |" EntS7072.InsuredSurname   SKIP.
      PUT STREAM xmlstream "30 PolicyTypeCd    |" EntS7072.PolicyTypeCd     SKIP.
      PUT STREAM xmlstream "31 RateGroup       |" EntS7072.RateGroup        SKIP.
      PUT STREAM xmlstream "27 WrittenAmt      |" EntS7072.WrittenAmt       SKIP.
      PUT STREAM xmlstream "5  CMIPolicyNumber |" EntS7072.CMIPolicyNumber  FORMAT "x(20)" SKIP.
      PUT STREAM xmlstream "7  CMIDocumentUID  |" EntS7072.CMIDocumentUID   FORMAT "X(20)" SKIP.
      PUT STREAM xmlstream "7  CMIBarCodeNumber|" EntS7072.CMIBarCodeNumber FORMAT "X(20)" SKIP.
      PUT STREAM xmlstream "32 CMIPolicyTypeCd |" EntS7072.CMIPolicyTypeCd  SKIP.
      PUT STREAM xmlstream "33 CMIVehTypeCd    |" EntS7072.CMIVehTypeCd     SKIP.
      PUT STREAM xmlstream "34 CMIWrittenAmt   |" EntS7072.CMIWrittenAmt    SKIP.
/*[BLANK]*/                      
      PUT STREAM xmlstream "35 SERVICE_ID      |" EntS7072.SERVICE_ID  SKIP.
      PUT STREAM xmlstream  FILL("-",78) FORMAT "X(78)" SKIP(1).
      PUT STREAM xmlstream  "/***************************************************/" SKIP(1).
    END.
    OUTPUT STREAM xmlstream CLOSE.
  END.
END. /*IF nv_msgerrordupl = "" THEN DO:*/
ELSE DO:
  IF nv_RECIDEntS7072 <> 0 AND nv_RECIDEntS7072 <> ? THEN DO:
    RUN Pc_SAVEDUPL (nv_RECIDEntS7072, nv_DuplPolicy, nv_msgerrordupl).
  END.
END. /*IF nv_msgerrordupl = "" THEN DO:*/
nv_LongCount = 0.
RELEASE EntS7072Result.
RELEASE IntS7072.
RELEASE EntS7072.
RELEASE IntPol7072.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_CheckMsg C-Win 
PROCEDURE Pc_CheckMsg :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_msgtext   AS CHARACTER NO-UNDO.
OUTPUT STREAM xmlstream TO PUT_EntS7072.TXT  APPEND .
/*[BLANK]*/                        
    PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
      PUT STREAM xmlstream "Text   |" nv_msgtext      SKIP.   
      PUT STREAM xmlstream  "/***************************************************/" SKIP(1).
/*[BLANK]*/                          
    OUTPUT STREAM xmlstream CLOSE.
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVECKAGT C-Win 
PROCEDURE Pc_SAVECKAGT :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT         PARAMETER  nv_BrokerCompany  AS CHAR FORMAT "x(20)".
DEFINE INPUT-OUTPUT  PARAMETER  nv_chkerror       AS CHAR FORMAT "x(500)".  
/*[COMMENT]*/                    /*DEF VAR nv_BrokerCompany AS CHAR FORMAT "x(12)" INIT "". */
DEF VAR nv_agent         AS CHAR FORMAT "x(20)" INIT "".
DEF VAR nv_producer      AS CHAR FORMAT "x(20)" INIT "" .
/*[COMMENT]*/                    /*DEF VAR nv_chkerror      AS CHAR FORMAT "x(500)" . */
DEF BUFFER bxmm600       FOR  sic_bran.xmm600.
/*[BLANK]*/                      
/*[BLANK]*/                           
FIND LAST  stat.company USE-INDEX Company01 WHERE
    stat.company.Compno = nv_BrokerCompany  /*ºÃÔÉÑ·ã¹ÃÐºº*/
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAIL stat.company THEN 
    ASSIGN
    nv_agent         = stat.company.Agent
    nv_producer      = stat.company.Acno1.
ELSE DO:
        OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
        PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
            " äÁè¾ºÃËÑÊºÃÔÉÑ·: "  nv_BrokerCompany   FORMAT "x(100)" SKIP.
        OUTPUT CLOSE.
END.
FIND LAST sic_bran.xmm600 WHERE xmm600.acno = TRIM(nv_agent) NO-LOCK NO-ERROR.
IF AVAIL sic_bran.xmm600 THEN DO:
/*[COMMENT]*/                        /* àªç¤ lincen Agent code */
    IF sic_bran.xmm600.agtreg = "" THEN DO:  /* agent äÁèÁÕ licen */
        ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
        OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
        PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
            nv_agent  " Agent Licence no. is Null "  FORMAT "x(100)" SKIP.
        OUTPUT CLOSE.
    END.
    ELSE IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN DO:  
/*[COMMENT]*/                            /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
        ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
        OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
        PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
            nv_agent  " Agent Licence Expire Date: " STRING(sic_bran.xmm600.regdate,"99/99/9999") FORMAT "x(100)" SKIP.
        OUTPUT CLOSE.
    END.
    ELSE IF sic_bran.xmm600.iblack <> "" THEN DO:  /* check over credit */
        ASSIGN nv_chkerror = nv_chkerror + "| Agent code has OVER CREDIT " .
        OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
        PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
            nv_agent  " Agent code has OVER CREDIT"  FORMAT "x(100)" SKIP.
        OUTPUT CLOSE.
    END.
/*[COMMENT]*/                        /*
/*[COMMENT]*/                        ELSE IF sic_bran.xmm600.clicod = "DI" THEN DO:  /* agent = DI */
/*[COMMENT]*/                            FIND LAST bxmm600 WHERE bxmm600.acno = TRIM(nv_producer) NO-LOCK NO-ERROR.
/*[COMMENT]*/                            IF AVAIL bxmm600 THEN DO:
/*[COMMENT]*/                                IF bxmm600.clicod <> "DI" THEN DO: /* Producer <> DI */
/*[COMMENT]*/                                    FIND LAST sic_bran.acno_fil WHERE sic_bran.acno_fil.acno = bxmm600.acno NO-LOCK NO-ERROR . /* àªç¤¢éÍÁÙÅ·Õè acno_fil */
/*[COMMENT]*/                                        IF NOT AVAIL sic_bran.acno_fil THEN /* äÁèÁÕ¢éÍÁÙÅ·Õè acno_fil */
/*[COMMENT]*/                                            ASSIGN nv_chkerror = nv_chkerror + "| Agent Client type Equals 'DI'/ Producer Client type Not Equals 'DI' and not found on Parameter(acno_fil)".
/*[COMMENT]*/                                        ELSE DO: 
/*[COMMENT]*/                                            /* àªç¤ÇÑ¹·Õè»Ô´â¤é´ ¹éÍÂ¡ÇèÒ ÇÑ¹·Õè¤ØéÁ¤ÃÍ§ */
/*[COMMENT]*/                                            IF bxmm600.closed <> ? AND bxmm600.closed < TODAY THEN DO: 
/*[COMMENT]*/                                                ASSIGN nv_chkerror = nv_chkerror + "| Producer Code Closed Date: " + STRING(bxmm600.closed,"99/99/9999") .
/*[COMMENT]*/                                            END.
/*[COMMENT]*/                                            ELSE DO: 
/*[COMMENT]*/                                                /* ÂÑ§äÁè»Ô´â¤é´ àªç¤ Over Credit */
/*[COMMENT]*/                                                IF bxmm600.iblack <> "" THEN ASSIGN nv_chkerror = nv_chkerror + "| Producer code has OVER CREDIT " .
/*[COMMENT]*/                                                ELSE DO: 
/*[COMMENT]*/                                                    /* äÁèµÔ´ Over credit */
/*[COMMENT]*/                                                    IF bxmm600.agtreg = "" THEN DO: /* producer code äÁèÁÕ licen */
/*[COMMENT]*/                                                        /* àªç¤ lincen Agent code */
/*[COMMENT]*/                                                        IF sic_bran.xmm600.agtreg = "" THEN  /* agent äÁèÁÕ licen */
/*[COMMENT]*/                                                            ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
/*[COMMENT]*/                                                        ELSE DO: /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
/*[COMMENT]*/                                                            IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN
/*[COMMENT]*/                                                            ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
/*[COMMENT]*/                                                        END.
/*[COMMENT]*/                                                    END.
/*[COMMENT]*/                                                    ELSE DO: /* producer code ÁÕ licen check licen expire */
/*[COMMENT]*/                                                        IF bxmm600.regdate <> ? AND bxmm600.regdate < TODAY THEN
/*[COMMENT]*/                                                            ASSIGN nv_chkerror = nv_chkerror + "| Producer Licence Expire Date: " + STRING(bxmm600.regdate,"99/99/9999") .
/*[COMMENT]*/                                                    END.
/*[COMMENT]*/                                                END.
/*[COMMENT]*/                                            END.
/*[COMMENT]*/                                        END.
/*[COMMENT]*/                                END.
/*[COMMENT]*/                                ELSE DO: /* Producer code = DI */
/*[COMMENT]*/                                    /* àªç¤ÇÑ¹·Õè»Ô´â¤é´ ¹éÍÂ¡ÇèÒ ÇÑ¹·Õè¤ØéÁ¤ÃÍ§ */
/*[COMMENT]*/                                    IF bxmm600.closed <> ? AND bxmm600.closed < TODAY THEN DO: 
/*[COMMENT]*/                                        ASSIGN nv_chkerror = nv_chkerror + "| Producer Code Closed Date: " + STRING(bxmm600.closed,"99/99/9999") .
/*[COMMENT]*/                                    END.
/*[COMMENT]*/                                    ELSE DO: 
/*[COMMENT]*/                                        /* ÂÑ§äÁè»Ô´â¤é´ àªç¤ Over Credit */
/*[COMMENT]*/                                        IF bxmm600.iblack <> "" THEN ASSIGN nv_chkerror = nv_chkerror + "| Producer code has OVER CREDIT " .
/*[COMMENT]*/                                        ELSE DO: 
/*[COMMENT]*/                                            /* äÁèµÔ´ Over credit */
/*[COMMENT]*/                                            IF bxmm600.agtreg = "" THEN DO: /* producer code äÁèÁÕ licen */
/*[COMMENT]*/                                                /* àªç¤ lincen Agent code */
/*[COMMENT]*/                                                IF sic_bran.xmm600.agtreg = "" THEN  /* agent äÁèÁÕ licen */
/*[COMMENT]*/                                                    ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
/*[COMMENT]*/                                                ELSE DO: /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
/*[COMMENT]*/                                                    IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN
/*[COMMENT]*/                                                    ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
/*[COMMENT]*/                                                END.
/*[COMMENT]*/                                            END.
/*[COMMENT]*/                                            ELSE DO: /* producer code ÁÕ licen check licen expire */
/*[COMMENT]*/                                                IF bxmm600.regdate <> ? AND bxmm600.regdate < TODAY THEN
/*[COMMENT]*/                                                    ASSIGN nv_chkerror = nv_chkerror + "| Producer Licence Expire Date: " + STRING(bxmm600.regdate,"99/99/9999") .
/*[COMMENT]*/                                            END.
/*[COMMENT]*/                                        END.
/*[COMMENT]*/                                    END.
/*[COMMENT]*/                                END. /* else producer = DI */
/*[COMMENT]*/                            END. /* avail bxmm600 */
/*[COMMENT]*/                            ELSE DO:
/*[COMMENT]*/                                ASSIGN nv_chkerror = nv_chkerror + "|Not found Producer Code " + nv_producer  + " ·Õèxmm600 ".
/*[COMMENT]*/                            END.
/*[COMMENT]*/                        END. /* agent  = DI */
/*[COMMENT]*/                        */
    ELSE DO: /* Agent code Client type <> DI ,Check Producer */
        FIND LAST bxmm600 WHERE bxmm600.acno = TRIM(nv_producer) NO-LOCK NO-ERROR.
        IF AVAIL bxmm600 THEN DO:
/*[COMMENT]*/                                /* àªç¤ÇÑ¹·Õè»Ô´â¤é´ ¹éÍÂ¡ÇèÒ ÇÑ¹·Õè¤ØéÁ¤ÃÍ§ */
            IF bxmm600.closed <> ? AND bxmm600.closed < TODAY THEN DO: 
                ASSIGN nv_chkerror = nv_chkerror + "| Producer Code Closed Date: " + STRING(bxmm600.closed,"99/99/9999") .
                OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
                PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                    nv_producer  " Producer Code Closed Date: " STRING(bxmm600.closed,"99/99/9999") FORMAT "x(100)" SKIP.
                OUTPUT CLOSE.
            END.
            ELSE DO: 
/*[COMMENT]*/                                    /* ÂÑ§äÁè»Ô´â¤é´ àªç¤ Over Credit */
                IF bxmm600.iblack <> "" THEN DO: 
                    ASSIGN nv_chkerror = nv_chkerror + "| Producer code has OVER CREDIT " .
                    OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
                    PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                        nv_producer  " Producer code has OVER CREDIT"  FORMAT "x(100)" SKIP.
                    OUTPUT CLOSE.
                END.
                ELSE DO: 
/*[COMMENT]*/                                        /* äÁèµÔ´ Over credit */
                    IF bxmm600.agtreg = "" THEN DO: /* producer code äÁèÁÕ licen */
/*[COMMENT]*/                                            /* àªç¤ lincen Agent code */
                        IF sic_bran.xmm600.agtreg = "" THEN DO:  /* agent äÁèÁÕ licen */
                            ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
                            OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
                            PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                                nv_producer  " Agent Licence no. is Null"  FORMAT "x(100)" SKIP.
                            OUTPUT CLOSE.
                        END.
                        ELSE DO: /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
                            IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN DO:
                                ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
                                OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
                                PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                                    nv_producer  " Agent Licence Expire Date: "   STRING(sic_bran.xmm600.regdate,"99/99/9999")  FORMAT "x(100)" SKIP.
                                OUTPUT CLOSE.
                            END.
                        END.
                    END.
                    ELSE DO: /* producer code ÁÕ licen check licen expire */
                        IF bxmm600.regdate <> ? AND bxmm600.regdate < TODAY THEN DO:
                            ASSIGN nv_chkerror = nv_chkerror + "| Producer Licence Expire Date: " + STRING(bxmm600.regdate,"99/99/9999") .
                            OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
                            PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                                nv_producer " Producer Licence Expire Date: "   STRING(bxmm600.regdate,"99/99/9999")  FORMAT "x(100)" SKIP.
                            OUTPUT CLOSE.
                        END.
                    END.
                END.
            END.
        END.
        ELSE DO: 
            ASSIGN nv_chkerror = nv_chkerror + "| Not found Producer Code " + nv_producer  + " ·Õèxmm600 ".
            OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
            PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
                " Not found Producer Code "  nv_producer    FORMAT "x(100)" SKIP.
            OUTPUT CLOSE.
        END.
    END.
END.
ELSE DO: 
    ASSIGN nv_chkerror = nv_chkerror + "| Not found Agent Code " + nv_agent + " ·Õèxmm600 ".
    OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
    PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
        " Not found Agent Code "   nv_agent    FORMAT "x(100)" SKIP.
    OUTPUT CLOSE.
END.
/*[BLANK]*/                       
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVECKGrp C-Win 
PROCEDURE Pc_SAVECKGrp :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER nv_racidck  AS RECID     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_msgerrorgrp   AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
FIND EntS7072 WHERE RECID(EntS7072) = nv_racidck
NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE EntS7072 THEN DO:
/*[COMMENT]*/                        /*§Ò¹·ÕèäÁèµéÍ§µÃÇ¨ÊÀÒ¾ ÅçÍ¤·Ñé§ËÁ´¤èÐ Â¡àÇé¹¾Ãº.áÅÐ»ÃÐàÀ· 3  ÍéÒ§ÍÔ§¨Ò¡¾ÃÕàÁÕèÂÁä´éàÅÂ ·Ø¡ÃØè¹¤èÐ àËÁ×Í¹¡ÅØèÁ 1 
/*[COMMENT]*/                                 2.2 áÅÐ 3.2  */
    IF   trim(EntS7072.Manufacturer) = "PROTON" THEN DO:
        IF EntS7072.CMIPolicyTypeCd = ""  THEN  /*V72 not check */
            nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model + "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
    END.
    ELSE IF ((substr(EntS7072.PolicyTypeCd,1,1)  = "2" ) AND  (  deci(EntS7072.COLLAmtAccident) < 350001 )  ) OR 
        (EntS7072.PolicyTypeCd = "3.1")  OR (EntS7072.PolicyTypeCd = "3.2") THEN DO:
/*[BLANK]*/                              
        FIND LAST FVehGrp USE-INDEX FVehGrp01 WHERE 
            FVehGrp.MakDes  = EntS7072.Manufacturer  AND     
            FVehGrp.ModDes  = EntS7072.Model         NO-LOCK NO-ERROR NO-WAIT.
        IF AVAIL FVehGrp  THEN DO:
            IF FVehGrp.Vehgrp  = "1" OR FVehGrp.Vehgrp = "2" THEN
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ " + FVehGrp.Vehgrp + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[BLANK]*/                                  
        END.
/*[COMMENT]*/                            /*ELSE nv_msgerrorgrp  = "".*/
        IF nv_msgerrorgrp  = ""  THEN DO:
/*[BLANK]*/                               
            IF   EntS7072.Manufacturer = "TOYOTA"  AND 
                (EntS7072.Model = "BB"       OR EntS7072.Model = "Estima"       OR
                 EntS7072.Model = "Harrier"  OR EntS7072.Model = "Land Cruiser" OR 
                 EntS7072.Model = "Vellfire" OR EntS7072.Model = "Alphard"      OR
                 EntS7072.Model = "Voxy"     OR EntS7072.Model = "Crown"        OR
                 EntS7072.Model = "Vitz"     OR EntS7072.Model = "ALtezza"      OR EntS7072.Model = "COMMUTER"  ) THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF EntS7072.Manufacturer = "Chevrolet" AND
                (EntS7072.Model = "Camero" OR EntS7072.Model = "Express") THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF EntS7072.Manufacturer = "Nissan" AND
                (EntS7072.Model = "Cube"     OR EntS7072.Model = "Fairlady" OR EntS7072.Model = "GT-R") THEN
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF EntS7072.Manufacturer = "Honda"  AND EntS7072.Model = "S660"    THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF EntS7072.Manufacturer = "Suzuki" AND EntS7072.Model = "Lapin"   THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF EntS7072.Manufacturer = "Ford"   AND EntS7072.Model = "Tunland" THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE IF index(EntS7072.Manufacturer,"BENZ") <> 0  THEN 
                nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
                                  " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
                                  "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
            ELSE nv_msgerrorgrp  = "".
/*[BLANK]*/                      
        END.
    END.
    ELSE nv_msgerrorgrp  = "".
    IF EntS7072.Manufacturer = "Chevrolet" THEN DO:
        IF (trim(EntS7072.PolicyTypeCd)  = "1"   OR trim(EntS7072.PolicyTypeCd)  = "2.1" OR trim(EntS7072.PolicyTypeCd)  = "2.2" OR
            trim(EntS7072.PolicyTypeCd)  = "3.1" OR trim(EntS7072.PolicyTypeCd)  = "3.2") AND 
            ((YEAR(today) - deci(EntS7072.ModelYear)) + 1 > 5 ) THEN
            nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model + "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹ ÍÒÂØÃ¶à¡Ô¹ 5 »Õ".
    END.
/*[BLANK]*/                      
END.
IF nv_msgerrorgrp <> "" THEN DO:
    OUTPUT TO PC_SaveERRGroup12.txt APPEND.
    PUT "-------------------------" SKIP
        " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP
        "Error : " nv_msgerrorgrp  FORMAT "X(150)" SKIP.
    OUTPUT CLOSE.
END.
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEDUPL C-Win 
PROCEDURE Pc_SAVEDUPL :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT PARAMETER nv_RECIDEntS7072  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER nv_DuplPolicy     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nv_msgerrordupl   AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_DuplContract          AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
IF nv_DuplPolicy <> "" THEN DO:
/*[BLANK]*/                      
  FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_DuplPolicy /*70*/
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE IntPol7072 THEN
     nv_DuplContract = IntPol7072.ContractNumber.
  ELSE DO:
/*[BLANK]*/                      
    FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_DuplPolicy /*72*/
    NO-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE IntPol7072 THEN
       nv_DuplContract = IntPol7072.ContractNumber.
  END.
END.
/*[BLANK]*/                      
/*[BLANK]*/                      
FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
NO-ERROR NO-WAIT.
IF AVAILABLE EntS7072 THEN DO:
/*[BLANK]*/                      
  FIND FIRST EntS7072Result WHERE
             EntS7072Result.SystemRq        = EntS7072.SystemRq
         AND EntS7072Result.CompanyCode     = EntS7072.CompanyCode
         AND EntS7072Result.ContractNumber  = EntS7072.ContractNumber
         AND EntS7072Result.keyRequestIndRq = EntS7072.keyRequestIndRq
  NO-ERROR NO-WAIT.
  IF NOT AVAILABLE EntS7072Result THEN  CREATE EntS7072Result.
/*[BLANK]*/                       
  ASSIGN
  EntS7072Result.CompanyCode          = EntS7072.CompanyCode
  EntS7072Result.ReferenceNumber      = EntS7072.ReferenceNumber
/*[COMMENT]*/                      /**/
  EntS7072Result.InsurerCode          = EntS7072.InsurerCode
  EntS7072Result.MethodCode           = EntS7072.MethodCode
  EntS7072Result.Policy               = EntS7072.Policy
  EntS7072Result.Rencnt               = EntS7072.Rencnt
  EntS7072Result.Endcnt               = EntS7072.Endcnt
  EntS7072Result.Riskno               = EntS7072.Riskno
  EntS7072Result.Itemno               = EntS7072.Itemno
/*[COMMENT]*/                      /**/
  EntS7072Result.ProcessByUser        = EntS7072.ProcessByUser
  EntS7072Result.ProcessDate          = EntS7072.ProcessDate
  EntS7072Result.ProcessTime          = EntS7072.ProcessTime
/*[COMMENT]*/                      /**/
  EntS7072Result.TrnFromIntDate       = EntS7072.TrnFromIntDate
  EntS7072Result.TrnFromIntTime       = EntS7072.TrnFromIntTime
  EntS7072Result.ReceiveNumber        = EntS7072.ReceiveNumber
/*[COMMENT]*/                      /**/
  EntS7072Result.SystemRq             = EntS7072.SystemRq
  EntS7072Result.InsurerId            = EntS7072.InsurerId
  EntS7072Result.ContractNumber       = EntS7072.ContractNumber
  EntS7072Result.RqUID                = EntS7072.RqUID
  EntS7072Result.keyRequestIndRq      = EntS7072.keyRequestIndRq
/*[BLANK]*/                       
  EntS7072Result.RecordGUIDRs         = ""
/*[COMMENT]*/                      /**/
  EntS7072Result.TransactionResponseDt   =   STRING( YEAR(EntS7072.TrnFromIntDate),"9999")
                                           + STRING(MONTH(EntS7072.TrnFromIntDate),"99")
                                           + STRING(  DAY(EntS7072.TrnFromIntDate),"99")
  EntS7072Result.TransactionResponseTime = EntS7072.TrnFromIntTime
/*[COMMENT]*/                      /**/
  EntS7072Result.PolicyNumber         = EntS7072.PolicyNumber
  EntS7072Result.DocumentUID          = EntS7072.DocumentUID
/*[COMMENT]*/                      /**/
  EntS7072Result.CMIPolicyNumber      = EntS7072.CMIPolicyNumber
  EntS7072Result.CMIDocumentUID       = EntS7072.CMIDocumentUID
  EntS7072Result.MsgStatusCd          = "FAIL"
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      EntS7072Result.CMIBarCodeNumber     = EntS7072.CMIBarCodeNumber
/*[COMMENT]*/                      */
/*[COMMENT]*/                      /**/
  EntS7072Result.vehreg               = EntS7072.vehreg
  EntS7072Result.Adjustno             = 0
/*[COMMENT]*/                      /* */
  EntS7072Result.ResultStatus         = "FAIL"                 /*EntS7072.ResultStatus*/
  EntS7072Result.ErrorCode            = ""
  EntS7072Result.ErrorMessage         = nv_msgerrordupl
/*[COMMENT]*/                      /* */
  EntS7072Result.LinkStatus           = "E"      .
/*[BLANK]*/                      
/*[BLANK]*/                      
  IF nv_DuplPolicy <> "" AND nv_DuplContract <> "" THEN DO:
/*[BLANK]*/                      
    EntS7072Result.EndorseRefNumber     = nv_DuplPolicy  .
    EntS7072Result.EndorseReceiveNumber = nv_DuplContract.
  END.
/*[BLANK]*/                      
  IF EntS7072.InsurerId = "" THEN EntS7072Result.InsurerId = EntS7072.CompanyCode.
/*[BLANK]*/                         
  EntS7072.ProcessStatus = "X" .
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ----------------------------------------------------------- */
/*[BLANK]*/                      
RELEASE EntS7072Result.
RELEASE EntS7072.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEFMRate C-Win 
PROCEDURE Pc_SAVEFMRate :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER nv_RECIDEntS7072  AS RECID     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_msgerrorgrp    AS CHARACTER NO-UNDO.
DEFINE VAR    nv_sumins  AS DECI INIT  0.
/*[BLANK]*/                      
IF nv_msgerrorgrp = "" THEN DO:
/*[BLANK]*/                          
FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE EntS7072 THEN DO:
    IF EntS7072.RateGroup <> "" THEN DO:
        IF EntS7072.RateGroup  <> "110"  AND EntS7072.RateGroup  <> "210" AND EntS7072.RateGroup  <> "320" AND EntS7072.RateGroup  <> "610" THEN 
            nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
        ELSE DO:
            IF      EntS7072.RateGroup  = "320" THEN DO:
                IF  deci(EntS7072.GrossVehOrCombinedWeight) > 4 THEN
                    nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¹éÓË¹Ñ¡Ã¶¤×Í " +   EntS7072.GrossVehOrCombinedWeight + "à¡Ô¹ 4 µÑ¹" .
            END.
            ELSE IF EntS7072.RateGroup  = "210" THEN DO: 
                IF INTE(EntS7072.SeatingCapacity) > 12 THEN
                    nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¨Ó¹Ç¹·Õè¹Ñè§" +   EntS7072.SeatingCapacity + "à¡Ô¹ 12 ·Õè¹Ñè§" .
            END.
            ELSE IF EntS7072.RateGroup  = "110" THEN DO: 
                IF INTE(EntS7072.SeatingCapacity) > 7 THEN
                    nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¨Ó¹Ç¹·Õè¹Ñè§" +   EntS7072.SeatingCapacity + "à¡Ô¹ 7 ·Õè¹Ñè§" .
            END.
            ELSE nv_msgerrorgrp = "äÁè¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
        END.
    END.
    IF nv_msgerrorgrp = "" THEN DO:
        IF EntS7072.Model = "commuter" THEN DO:
            IF substr(TRIM(EntS7072.Registration),1,1) >= "0" AND  substr(TRIM(EntS7072.Registration),1,1) <= "9" THEN DO:
                IF substr(TRIM(EntS7072.Registration),2,1) >= "0" AND  substr(TRIM(EntS7072.Registration),2,1) <= "9" THEN 
                    nv_msgerrorgrp = "¾º·ÐàºÕÂ¹Ã¶à»ç¹µÑÇàÅ¢ : " + EntS7072.Registration + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
            END.
        END.
    END.
    IF nv_msgerrorgrp = "" THEN DO:
        IF EntS7072.PolicyStatus = "N" /*AND EntS7072.CompanyCode <> "834"*/   THEN DO:
            nv_sumins = deci(EntS7072.COLLAmtAccident).
            IF  EntS7072.RateGroup  <> "" AND EntS7072.PolicyTypeCd <> "" AND EntS7072.PolicyTypeCd <> "1" THEN DO:
                IF EntS7072.PolicyTypeCd = "3" THEN DO:
                    IF EntS7072.CurrentTermAmt <> ""  THEN DO:
                        FIND FIRST FMRate WHERE
/*[COMMENT]*/                                                /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
                            FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/ 
                            FMRate.EffDate    <= TODAY                   AND
                            FMRate.SClass     = EntS7072.RateGroup       AND
                            FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
                        IF NOT AVAILABLE FMRate THEN DO:
                            FIND FIRST FMRate3 WHERE
                                FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
                                FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                                FMRate3.EffDate    <= TODAY                   AND
                                FMRate3.SClass      = EntS7072.RateGroup       AND
                                FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
                                NO-LOCK NO-ERROR NO-WAIT.
                            IF NOT AVAILABLE FMRate THEN DO:
                                nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
                                    " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
                                    " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
                            END.                                              
                        END.
/*[BLANK]*/                      
                    END.
                    ELSE DO:  
                        IF EntS7072.CurrentTermAmt = "" THEN DO:
                            FIND FIRST FMRate WHERE
                                FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/
/*[COMMENT]*/                                                    /*  FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/*/
                                FMRate.EffDate    <= TODAY                   AND
                                FMRate.SClass     = EntS7072.RateGroup       AND
                                FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
                            IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                                    /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                                    IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                                    RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                                    END.*/
/*[COMMENT]*/                                                    /*DISP  FMRate.SClass  FMRate.MinSI. */
                                FIND FIRST FMRate3 WHERE
                                    FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
                                    FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                                    FMRate3.EffDate    <= TODAY                   AND
                                    FMRate3.SClass      = EntS7072.RateGroup       AND
                                    FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
                                    NO-LOCK NO-ERROR NO-WAIT.
                                IF NOT AVAILABLE FMRate THEN DO:
                                    nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
                                        " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
                                        " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
                                END.                                              
                            END.
                        END.
                        ELSE DO:
                            FIND FIRST FMRate WHERE
/*[COMMENT]*/                                                    /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
                                FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/ 
                                FMRate.EffDate    <= TODAY                   AND
                                FMRate.SClass     = EntS7072.RateGroup       AND
                                FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
                            IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                                    /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                                    IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                                    RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                                    END.*/
/*[COMMENT]*/                                                    /*DISP  FMRate.SClass  FMRate.MinSI. */
                                FIND FIRST FMRate3 WHERE
                                    FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
                                    FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                                    FMRate3.EffDate    <= TODAY                   AND
                                    FMRate3.SClass      = EntS7072.RateGroup       AND
                                    FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
                                    NO-LOCK NO-ERROR NO-WAIT.
                                IF NOT AVAILABLE FMRate THEN DO:
                                    nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
                                        " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
                                        " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
                                END.                                              
                            END.
/*[BLANK]*/                      
                        END.
                    END.
                    IF EntS7072.CompanyCode = "M82" AND nv_msgerrorgrp <> "" THEN DO:
                        FIND FIRST FMRate WHERE
/*[COMMENT]*/                                            /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
                        FMRate.prm_gap    = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                        FMRate.EffDate    <= TODAY                   AND
                        FMRate.SClass     = EntS7072.RateGroup       AND
                        FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
                        IF AVAILABLE FMRate THEN   nv_msgerrorgrp = "".
                    END.
                END.
                ELSE DO:
                    FIND FIRST FMRate WHERE
/*[COMMENT]*/                                            /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
                        FMRate.prm_gap    = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                        FMRate.EffDate    <= TODAY                   AND
                        FMRate.SClass     = EntS7072.RateGroup       AND
                        FMRate.CoverCode  = EntS7072.PolicyTypeCd    AND
                        FMRate.MinSI      >= nv_sumins                      AND
                        FMRate.MaxSI      <= nv_sumins
                        NO-LOCK NO-ERROR NO-WAIT.
                    IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                            /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                            IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                            RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                            END.*/
/*[COMMENT]*/                                            /*DISP  FMRate.SClass  FMRate.MinSI. */
                        FIND FIRST FMRate3 WHERE
/*[COMMENT]*/                                                /*FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/*/
                            FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
                            FMRate3.EffDate    <= TODAY                   AND
                            FMRate3.SClass      = EntS7072.RateGroup       AND
                            FMRate3.CoverCode   = EntS7072.PolicyTypeCd    AND
                            FMRate3.MinSI      >= nv_sumins                     AND
                            FMRate3.MaxSI      <= nv_sumins
                            NO-LOCK NO-ERROR NO-WAIT.
                        IF NOT AVAILABLE FMRate THEN DO:
                            nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
                                " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
                                " ·Ø¹ :" + EntS7072.COLLAmtAccident + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd .
                        END.
                    END.
                END.
            END.
        END.
    END.
    IF nv_msgerrorgrp <> "" THEN DO:
        OUTPUT TO CK_NotFMRateERROR.TXT APPEND .
        PUT "/-----------------------------------------------/ "  SKIP. 
        PUT  TODAY FORMAT "99/99/9999" 
            " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
        PUT "Company        : " EntS7072.CompanyCode        FORMAT "X(10)" SKIP. 
        PUT "ContractNumber : " EntS7072.ContractNumber     FORMAT "X(35)" SKIP.   
        PUT "PolicyTypeCd   : " EntS7072.PolicyTypeCd       FORMAT "X(10)" SKIP.   
        PUT "RateGroup      : " EntS7072.RateGroup          FORMAT "X(10)" SKIP.   
        PUT "WrittenAmt     : " EntS7072.WrittenAmt         FORMAT "x(20)" SKIP.   
        PUT "CurrentTermAmt : " EntS7072.CurrentTermAmt     FORMAT "x(20)" SKIP.   
        PUT "SumInsureAmt   : " EntS7072.SumInsureAmt       FORMAT "X(20)" SKIP.   
        PUT "COLLAmtAccident: " EntS7072.COLLAmtAccident    FORMAT "X(20)" SKIP.   
        PUT "FTAmt          : " EntS7072.FTAmt              FORMAT "X(20)" SKIP.  
        PUT "Error :"           nv_msgerrorgrp              FORMAT "X(150)" SKIP.  
        OUTPUT  CLOSE.
    END.
END.
END.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SavePDFLKT C-Win 
PROCEDURE Pc_SavePDFLKT :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*add kridtiya i. A58-0356 */
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq NO-ERROR NO-WAIT.
IF AVAIL IntS7072 THEN DO:
    IF IntS7072.CompanyCode = "469" THEN DO:
        CREATE ExtPDFLKT70.
            ASSIGN 
                ExtPDFLKT70.SystemRq        = "Lockton" 
                ExtPDFLKT70.ContractNumber  = IntS7072.ContractNumber 
                ExtPDFLKT70.PolicyNumber    = IntS7072.Policy
                ExtPDFLKT70.FileNameAttach1 = IntS7072.FileNameAttach1        
                ExtPDFLKT70.FileNameAttach2 = IntS7072.FileNameAttach2        
                ExtPDFLKT70.FileNameAttach3 = IntS7072.FileNameAttach3        
                ExtPDFLKT70.FileNameAttach4 = IntS7072.FileNameAttach4        
                ExtPDFLKT70.FileNameAttach5 = IntS7072.FileNameAttach5        
                ExtPDFLKT70.FileNameAttach6 = IntS7072.FileNameAttach6  
                ExtPDFLKT70.AttachFile1     = IntS7072.AttachFile1         
                ExtPDFLKT70.AttachFile2     = IntS7072.AttachFile2         
                ExtPDFLKT70.AttachFile3     = IntS7072.AttachFile3         
                ExtPDFLKT70.AttachFile4     = IntS7072.AttachFile4         
                ExtPDFLKT70.AttachFile5     = IntS7072.AttachFile5         
                ExtPDFLKT70.AttachFile6     = IntS7072.AttachFile6 .
/*[BLANK]*/                                  
/*[BLANK]*/                              
/*[BLANK]*/                      
    END.
END.
RELEASE ExtPDFLKT70.
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEPDResult C-Win 
PROCEDURE Pc_SAVEPDResult :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:      ãªéÊÓËÃÑº µÍº¡ÅÑº àªè¹ ·Ø¹ àºÕéÂ ÍÒ¡Ã ÀÒÉÕ 
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
ASSIGN
EntS7072Result.PolicyTypeCd                   = IntS7072.PolicyTypeCd
EntS7072Result.RateGroup                      = IntS7072.RateGroup
EntS7072Result.GarageTypeCd                   = IntS7072.GarageTypeCd
EntS7072Result.GarageDesc                     = IntS7072.GarageDesc
EntS7072Result.SumInsureAmt                   = IntS7072.SumInsureAmt
EntS7072Result.TPBIAmtPerson                  = IntS7072.TPBIAmtPerson
EntS7072Result.TPBIAmtAccident                = IntS7072.TPBIAmtAccident
EntS7072Result.PDAmtAccident                  = IntS7072.PDAmtAccident
EntS7072Result.DeductiblePDAmtAccident        = IntS7072.DeductiblePDAmtAccident
EntS7072Result.COLLAmtAccident                = IntS7072.COLLAmtAccident
EntS7072Result.DeductibleCOLLAmtAccident      = IntS7072.DeductibleCOLLAmtAccident
EntS7072Result.FTAmt                          = IntS7072.FTAmt
EntS7072Result.PerilsPADriverAmt              = IntS7072.PerilsPADriverAmt
EntS7072Result.PerilsPANumPassengers          = IntS7072.PerilsPANumPassengers
EntS7072Result.PerilsPAPassengerAmt           = IntS7072.PerilsPAPassengerAmt
EntS7072Result.PerilsPATemporaryDriverAmt     = IntS7072.PerilsPATemporaryDriverAmt
EntS7072Result.PerilsPANumTemporaryPassengers = IntS7072.PerilsPANumTemporaryPassengers
EntS7072Result.PerilsPATemporaryPassengerAmt  = IntS7072.PerilsPATemporaryPassengerAmt
EntS7072Result.PerilsMedicalTreatmentAmt      = IntS7072.PerilsMedicalTreatmentAmt
EntS7072Result.PerilsBailBondInsuranceAmt     = IntS7072.PerilsBailBondInsuranceAmt
EntS7072Result.PremiumCoverage13Amt           = IntS7072.PremiumCoverage13Amt
EntS7072Result.DiscountForNamedDriver         = IntS7072.DiscountForNamedDriver
EntS7072Result.DeductibleAmt                  = IntS7072.DeductibleAmt
EntS7072Result.FleetAmt                       = IntS7072.FleetAmt
EntS7072Result.GoodDriverIndPct               = IntS7072.GoodDriverIndPct
EntS7072Result.GoodDriverIndAmt               = IntS7072.GoodDriverIndAmt
EntS7072Result.TotalDiscountsAmt              = IntS7072.TotalDiscountsAmt
EntS7072Result.SurchargeFactorAmt             = IntS7072.SurchargeFactorAmt
EntS7072Result.PremiumCoverage2Amt            = IntS7072.PremiumCoverage2Amt
EntS7072Result.OtherDiscountAmt               = IntS7072.OtherDiscountAmt
EntS7072Result.VehicleUse                     = IntS7072.VehicleUse
EntS7072Result.WrittenAmt                     = IntS7072.WrittenAmt
EntS7072Result.RevenueStampAmt                = IntS7072.RevenueStampAmt
EntS7072Result.VatAmt                         = IntS7072.VatAmt
EntS7072Result.CurrentTermAmt                 = IntS7072.CurrentTermAmt
/*[BLANK]*/                      
EntS7072Result.CMIPolicyTypeCd                = IntS7072.CMIPolicyTypeCd
EntS7072Result.CMIVehTypeCd                   = IntS7072.CMIVehTypeCd
EntS7072Result.CMIAmtPerson                   = IntS7072.CMIAmtPerson
EntS7072Result.CMIAmtAccident                 = IntS7072.CMIAmtAccident
EntS7072Result.CMIWrittenAmt                  = IntS7072.CMIWrittenAmt
EntS7072Result.CMIRevenueStampAmt             = IntS7072.CMIRevenueStampAmt
EntS7072Result.CMIVatAmt                      = IntS7072.CMIVatAmt
EntS7072Result.CMICurrentTermAmt              = IntS7072.CMICurrentTermAmt .
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEProduction C-Win 
PROCEDURE Pc_SAVEProduction :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_AddData        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_Cpolicy        AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_CBarCodeNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_ChkCpLink      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_CallbyLink     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_LinkFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_SwithChk       AS LOGICAL   NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_recinout7072   AS RECID     NO-UNDO.
DEFINE VARIABLE nv_SAVEERROR      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_companyno      AS CHARACTER INIT "".
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
ASSIGN
    nv_companyno  = ""
nv_SAVEERROR      = ""
nv_Cpolicy        = ""
nv_CBarCodeNumber = ""
nv_resulttext     = "".
/*[BLANK]*/                      
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF AVAILABLE IntS7072 THEN DO:
  ASSIGN   
    nv_companyno = IntS7072.CompanyCode
    nv_AddData   = NO.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* PolicyNumber     = ¡ÃÁ¸ÃÃÁì V70
/*[COMMENT]*/                         CMIPolicyNumber  = ¡ÃÁ¸ÃÃÁì ¾Ãº.
/*[BLANK]*/                           
/*[COMMENT]*/                         * ¶éÒà»ç¹¢éÍÁÙÅ·ÕèÊè§à©¾ÒÐ ¾Ãº.
/*[COMMENT]*/                             PolicyNumber     = ¡ÃÁ¸ÃÃÁì ¾Ãº.
/*[COMMENT]*/                             CMIPolicyNumber  = ¡ÃÁ¸ÃÃÁì ¾Ãº.
/*[BLANK]*/                               
/*[COMMENT]*/                         * ¶éÒà»ç¹¢éÍÁÙÅ·ÕèÊè§à©¾ÒÐ V70
/*[COMMENT]*/                             PolicyNumber     = ¡ÃÁ¸ÃÃÁì V70
/*[COMMENT]*/                             CMIPolicyNumber  = ""
/*[COMMENT]*/                      */
  IF nv_PolicyV70 = "" THEN nv_PolicyV70 = nv_PolicyV72.
/*[BLANK]*/                      
  FIND FIRST IntPol7072 WHERE
             IntPol7072.PolicyNumber = nv_PolicyV70  /*IntS7072.PolicyNumber*/
         AND IntPol7072.CompanyCode  = IntS7072.CompanyCode
/*[BLANK]*/                      
  NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
    nv_AddData = YES.
/*[COMMENT]*/                        /* ---
/*[COMMENT]*/                        FIND FIRST IntPol7072 WHERE
/*[COMMENT]*/                                   IntPol7072.StickerNumber = IntS7072.StickerNumber
/*[COMMENT]*/                        NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                        IF NOT AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          nv_AddData = YES.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          CREATE IntPol7072.
/*[COMMENT]*/                          */
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE nv_CBarCodeNumber = "BarCodeNumber".
/*[COMMENT]*/                        --- */
/*[BLANK]*/                      
  END.
  ELSE DO:
/*[BLANK]*/                          
    ASSIGN 
/*[COMMENT]*/                        /* IntS7072.Statusflag = "U"  UPDATE DATA*/
    nv_Cpolicy = "PolicyNumber" 
/*[COMMENT]*/                        /* IF IntS7072.Statusflag <> "" THEN  */
    nv_AddData = YES.
  END.
/*[BLANK]*/                      
  IF nv_AddData = YES THEN DO:
    nv_covcodtyp1 = ""  . /*Add Kridtiya i. clear */
    nv_recinout7072 = 0.
    RUN PD_SAVEPD1 (nv_rec_rq
                   ,nv_PolicyV70 /*,nv_DocnoV70*/
                   ,nv_PolicyV72 /*,nv_DocnoV72*/
                   ,INPUT-OUTPUT nv_recinout7072).
/*[BLANK]*/                      
    IF nv_companyno = "469" THEN RUN Pc_SavePDFLKT.  /*Kridtiya i. Date. 17/07/2017*/ 
/*[BLANK]*/                      
    FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
    NO-ERROR NO-WAIT.
/*[COMMENT]*/                        /**/
/*[BLANK]*/                      
    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_recinout7072
    NO-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
      IF IntPol7072.GenSicBranST = "ERROR" THEN DO:
        nv_SAVEERROR = IntPol7072.ErrorMessage.
        nv_AddData = NO.
      END.
/*[BLANK]*/                      
/*[COMMENT]*/                          /*§Ò¹ "R" µèÍÍÒÂØ->¡¸. ÁÕ¡ÒÃà»ÅÕèÂ¹àºÍÃì¢éÒ§ã¹*/
      IF TRIM(IntPol7072.PolicyNumber) <> "" THEN DO:
/*[BLANK]*/                      
        nv_PolicyV70 = IntPol7072.PolicyNumber.
      END.
    END.
  END. /*IF nv_AddData = YES THEN DO:*/
/*[COMMENT]*/                      /* ----------------------------------------------------------- */
/*[BLANK]*/                      
  IF nv_AddData = YES THEN DO:
/*[BLANK]*/                      
    FIND FIRST IntPol7072Result WHERE
               IntPol7072Result.CompanyCode  = IntS7072.CompanyCode
           AND IntPol7072Result.PolicyNumber = nv_PolicyV70
    NO-ERROR NO-WAIT.
    IF NOT AVAILABLE IntPol7072Result THEN  CREATE IntPol7072Result.
/*[BLANK]*/                      
    ASSIGN
    IntPol7072Result.CompanyCode      = IntS7072.CompanyCode
    IntPol7072Result.ReferenceNumber  = IntS7072.ReferenceNumber
    IntPol7072Result.EndorseRefNumber = IntS7072.EndorseRefNumber
/*[COMMENT]*/                        /**/
    IntPol7072Result.InsurerCode    = IntS7072.InsurerCode
    IntPol7072Result.MethodCode     = IntS7072.MethodCode
    IntPol7072Result.Policy         = IntS7072.Policy
    IntPol7072Result.Rencnt         = IntS7072.Rencnt
    IntPol7072Result.Endcnt         = IntS7072.Endcnt
    IntPol7072Result.Riskno         = IntS7072.Riskno
    IntPol7072Result.Itemno         = IntS7072.Itemno
/*[COMMENT]*/                        /**/
    IntPol7072Result.ProcessByUser  = IntS7072.ProcessByUser
    IntPol7072Result.ProcessDate    = IntS7072.ProcessDate
    IntPol7072Result.ProcessTime    = IntS7072.ProcessTime
/*[COMMENT]*/                        /**/
    IntPol7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
    IntPol7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
    IntPol7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
    IntPol7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[COMMENT]*/                        /**/
    IntPol7072Result.SystemRq        = IntS7072.SystemRq
    IntPol7072Result.InsurerId       = IntS7072.InsurerId
    IntPol7072Result.ContractNumber  = IntS7072.ContractNumber
    IntPol7072Result.RqUID           = nv_octets
    IntPol7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[BLANK]*/                      
    IntPol7072Result.RecordGUIDRs    = nv_octets
/*[COMMENT]*/                        /**/
    IntPol7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
                                             + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
                                             + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
    IntPol7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[COMMENT]*/                        /**/
    IntPol7072Result.PolicyNumber    = nv_PolicyV70
    IntPol7072Result.DocumentUID     = IntS7072.DocumentUID
/*[COMMENT]*/                        /**/
    IntPol7072Result.CMIPolicyNumber = nv_PolicyV72
    IntPol7072Result.CMIDocumentUID  = IntS7072.CMIDocumentUID
    IntPol7072Result.MsgStatusCd     = "SUCCESS" 
/*[BLANK]*/                          
/*[COMMENT]*/                        /*IntPol7072Result.CMIBarCodeNumber = IntS7072.CMIBarCodeNumber */
    IntPol7072Result.ResultStatus     = "SUCCESS"                 /*IntS7072.ResultStatus*/
    IntPol7072Result.ErrorCode        = ""
    IntPol7072Result.ErrorMessage     = nv_resulttext
/*[COMMENT]*/                        /**/
    IntPol7072Result.vehreg           = IntS7072.vehreg
    IntPol7072Result.Adjustno         = 0
    IntPol7072Result.ProcessStatus    = "X" .
/*[BLANK]*/                      
  END. /*IF nv_AddData = YES THEN DO:*/
/*[COMMENT]*/                      /* **
/*[COMMENT]*/                      IF nv_AddData = YES THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        IF CONNECTED ("BUInt203") THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          RUN WRS/WRSBQ7072P.P (nv_rec_rq
/*[COMMENT]*/                                               ,nv_PolicyV70
/*[COMMENT]*/                                               ,nv_PolicyV72).
/*[COMMENT]*/                        END.
/*[BLANK]*/                      
/*[COMMENT]*/                        FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[COMMENT]*/                        NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[COMMENT]*/                      END. /*IF nv_AddData = YES THEN DO:*/
/*[COMMENT]*/                      ** */
/*[COMMENT]*/                      /* ----------------------------------------------------------- */
/*[COMMENT]*/                      /* µÍº¡ÅÑº */
/*[BLANK]*/                      
  IF nv_AddData = YES THEN DO:
/*[BLANK]*/                      
    FIND FIRST EntS7072Result WHERE
               EntS7072Result.SystemRq     = IntS7072.SystemRq
           AND EntS7072Result.CompanyCode  = IntS7072.CompanyCode
           AND EntS7072Result.PolicyNumber = nv_PolicyV70
    NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          /*copy link*/
      nv_ChkCpLink = NO.
      nv_SwithChk  = NO.
/*[BLANK]*/                      
      IF IntS7072.FileNameAttach1 <> "" AND IntS7072.AttachFile1 <> ?
         AND IntS7072.CompanyCode <> "833" /*ÈÃÕ¡ÃØ§*/
      THEN DO:
/*[BLANK]*/                      
        FIND FIRST ExtInsurerCd WHERE
                   ExtInsurerCd.InsurerCd = IntS7072.CompanyCode
        NO-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE ExtInsurerCd THEN DO:
/*[BLANK]*/                      
          IF ExtInsurerCd.CallbyLink <> "" THEN DO: /*ÁÕ¡ÒÃ copy Link*/
            nv_ChkCpLink  = YES.
            nv_CallbyLink = ExtInsurerCd.CallbyLink.
/*[BLANK]*/                      
            RUN WRS/WRSCpLink.P (RECID(IntS7072)
                                ,nv_PolicyV70
                                ,nv_PolicyV72
                                ,nv_octets
                                ,nv_resulttext).
          END.
        END.
      END.
/*[COMMENT]*/                          /**/
/*[BLANK]*/                      
      FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
      NO-ERROR NO-WAIT.
/*[BLANK]*/                      
      CREATE EntS7072Result.
/*[BLANK]*/                      
      ASSIGN
      EntS7072Result.ReferenceNumber  = IntS7072.ReferenceNumber
      EntS7072Result.EndorseRefNumber = IntS7072.EndorseRefNumber
/*[BLANK]*/                      
      EntS7072Result.InsurerCode = IntS7072.InsurerCode
      EntS7072Result.MethodCode  = IntS7072.MethodCode
      EntS7072Result.Policy      = IntS7072.Policy
      EntS7072Result.Rencnt      = IntS7072.Rencnt
      EntS7072Result.Endcnt      = IntS7072.Endcnt
      EntS7072Result.Riskno      = IntS7072.Riskno
      EntS7072Result.Itemno      = IntS7072.Itemno
/*[BLANK]*/                      
      EntS7072Result.ProcessByUser = IntS7072.ProcessByUser
      EntS7072Result.ProcessDate   = IntS7072.ProcessDate
      EntS7072Result.ProcessTime   = IntS7072.ProcessTime
/*[BLANK]*/                      
      EntS7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
      EntS7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
      EntS7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
      EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
      EntS7072Result.RecordGUIDRs = nv_octets
/*[BLANK]*/                      
      EntS7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
                                             + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
                                             + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
      EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
      EntS7072Result.PolicyNumber     = nv_PolicyV70
      EntS7072Result.DocumentUID      = IntS7072.DocumentUID
      EntS7072Result.StickerNumber    = IntS7072.StickerNumber
      EntS7072Result.CMIPolicyNumber  = nv_PolicyV72
      EntS7072Result.CMIDocumentUID   = IntS7072.CMIDocumentUID
      EntS7072Result.CMIBarCodeNumber = IntS7072.CMIBarCodeNumber
/*[BLANK]*/                      
      EntS7072Result.MsgStatusCd  = "SUCCESS"
/*[BLANK]*/                      
      EntS7072Result.BranchCd     = IntS7072.BranchCd
      EntS7072Result.vehreg       = IntS7072.vehreg
      EntS7072Result.Adjustno     = 0
/*[BLANK]*/                      
      EntS7072Result.ResultStatus = "SUCCESS"                 /*IntS7072.ResultStatus*/
      EntS7072Result.ErrorCode    = ""
      EntS7072Result.ErrorMessage = nv_resulttext
/*[BLANK]*/                      
      EntS7072Result.AttachFile1  = IntS7072.AttachFile1
      EntS7072Result.AttachFile2  = IntS7072.AttachFile2
      EntS7072Result.AttachFile3  = IntS7072.AttachFile3
      EntS7072Result.AttachFile4  = IntS7072.AttachFile4
      EntS7072Result.AttachFile5  = IntS7072.AttachFile5
      EntS7072Result.AttachFile6  = IntS7072.AttachFile6
      EntS7072Result.AttachFile7  = IntS7072.AttachFile7
      EntS7072Result.AttachFile8  = IntS7072.AttachFile8
      EntS7072Result.AttachFile9  = IntS7072.AttachFile9
      EntS7072Result.AttachFile10 = IntS7072.AttachFile10
      EntS7072Result.FileNameAttach1  = IntS7072.FileNameAttach1
      EntS7072Result.FileNameAttach2  = IntS7072.FileNameAttach2
      EntS7072Result.FileNameAttach3  = IntS7072.FileNameAttach3
      EntS7072Result.FileNameAttach4  = IntS7072.FileNameAttach4
      EntS7072Result.FileNameAttach5  = IntS7072.FileNameAttach5
      EntS7072Result.FileNameAttach6  = IntS7072.FileNameAttach6
      EntS7072Result.FileNameAttach7  = IntS7072.FileNameAttach7
      EntS7072Result.FileNameAttach8  = IntS7072.FileNameAttach8
      EntS7072Result.FileNameAttach9  = IntS7072.FileNameAttach9
      EntS7072Result.FileNameAttach10 = IntS7072.FileNameAttach10
/*[BLANK]*/                      
      EntS7072Result.LinkStatus       = "" .
/*[BLANK]*/                      
      IF EntS7072Result.FileNameAttach1 <> "" THEN DO:
/*[BLANK]*/                      
        nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach1.  /*FILE NAME PDF*/
/*[BLANK]*/                      
        EntS7072Result.LinkPolicy = nv_LinkFile.
      END.
/*[BLANK]*/                      
      IF EntS7072Result.FileNameAttach2 <> "" THEN DO:
/*[BLANK]*/                      
        nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach2.  /*FILE NAME PDF*/
/*[BLANK]*/                      
        EntS7072Result.LinkFileAttach1 = nv_LinkFile.
      END.
/*[BLANK]*/                      
      IF EntS7072Result.FileNameAttach3 <> "" THEN DO:
/*[BLANK]*/                      
        nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach3.  /*FILE NAME PDF*/
/*[BLANK]*/                      
        EntS7072Result.LinkFileAttach2 = nv_LinkFile.
      END.
/*[COMMENT]*/                          /* ----------- */
/*[BLANK]*/                      
      RUN Pc_SAVEPDResult. /*µÍº¡ÅÑº ·Ø¹ àºÕéÂ ÍÒ¡Ã ÀÒÉÕ*/
/*[COMMENT]*/                          /* ----------- */
/*[BLANK]*/                      
/*[COMMENT]*/                          /*äÁèÁÕ¡ÒÃ copy Link*/
      IF nv_ChkCpLink = NO THEN EntS7072Result.LinkStatus = "X" .
/*[BLANK]*/                      
      ASSIGN
      EntS7072Result.SystemRq        = IntS7072.SystemRq
      EntS7072Result.CompanyCode     = IntS7072.CompanyCode
      EntS7072Result.InsurerId       = IntS7072.InsurerId
      EntS7072Result.ContractNumber  = IntS7072.ContractNumber
      EntS7072Result.RqUID           = nv_octets
      EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq .
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          FIND FIRST ExtInsurerCd WHERE
/*[COMMENT]*/                                     ExtInsurerCd.InsurerCd = EntS7072Result.CompanyCode
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                          IF AVAILABLE ExtInsurerCd THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                            IF ExtInsurerCd.CallbyLink = "" THEN DO: /*äÁèÁÕ¡ÒÃ copy Link*/
/*[BLANK]*/                      
/*[COMMENT]*/                              EntS7072Result.LinkStatus = "X" .
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          */
/*[COMMENT]*/                          /**/
    END.
/*[BLANK]*/                      
    FIND FIRST EntS7072 WHERE
               EntS7072.SystemRq        = IntS7072.SystemRq
           AND EntS7072.CompanyCode     = IntS7072.CompanyCode
           AND EntS7072.Username        = IntS7072.Username
           AND EntS7072.Password        = IntS7072.Password
           AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
    NO-ERROR NO-WAIT.
    IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "X" .
/*[BLANK]*/                      
    IntS7072.ProcessStatus = "X".
/*[BLANK]*/                      
  END. /*IF nv_AddData = YES THEN DO:*/
/*[BLANK]*/                      
  IF nv_AddData = NO THEN DO:  /*Duplication*/
/*[BLANK]*/                      
    FIND FIRST EntS7072Result WHERE
               EntS7072Result.SystemRq     = IntS7072.SystemRq
           AND EntS7072Result.CompanyCode  = IntS7072.CompanyCode
           AND EntS7072Result.PolicyNumber = nv_PolicyV70
    NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                      
      CREATE EntS7072Result.
/*[BLANK]*/                      
      ASSIGN
      EntS7072Result.ReferenceNumber = IntS7072.ReferenceNumber
      EntS7072Result.EndorseRefNumber= IntS7072.EndorseRefNumber
/*[BLANK]*/                      
      EntS7072Result.InsurerCode = IntS7072.InsurerCode
      EntS7072Result.MethodCode  = IntS7072.MethodCode
      EntS7072Result.Policy      = IntS7072.Policy
      EntS7072Result.Rencnt      = IntS7072.Rencnt
      EntS7072Result.Endcnt      = IntS7072.Endcnt
      EntS7072Result.Riskno      = IntS7072.Riskno
      EntS7072Result.Itemno      = IntS7072.Itemno
/*[BLANK]*/                      
      EntS7072Result.ProcessByUser = IntS7072.ProcessByUser
      EntS7072Result.ProcessDate   = IntS7072.ProcessDate
      EntS7072Result.ProcessTime   = IntS7072.ProcessTime
/*[BLANK]*/                      
      EntS7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
      EntS7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
      EntS7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
      EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
      EntS7072Result.RecordGUIDRs = ""
/*[BLANK]*/                      
      EntS7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
                                             + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
                                             + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
      EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
      EntS7072Result.PolicyNumber  = nv_PolicyV70
      EntS7072Result.DocumentUID   = ""
      EntS7072Result.StickerNumber = ""
/*[BLANK]*/                      
      EntS7072Result.CMIPolicyNumber  = nv_PolicyV72
      EntS7072Result.CMIDocumentUID   = ""
      EntS7072Result.CMIBarCodeNumber = "" 
      EntS7072Result.MsgStatusCd      = "FAIL"
/*[BLANK]*/                      
      EntS7072Result.BranchCd = IntS7072.BranchCd
      EntS7072Result.vehreg   = IntS7072.vehreg
      EntS7072Result.Adjustno = 0
/*[BLANK]*/                      
      EntS7072Result.ResultStatus = "FAIL"                 /*IntS7072.ResultStatus*/
      EntS7072Result.ErrorCode    = ""
      EntS7072Result.ErrorMessage = "" 
/*[BLANK]*/                      
      EntS7072Result.LinkStatus   = "E"     .
/*[BLANK]*/                      
      IF nv_Cpolicy <> "" THEN DO:
/*[BLANK]*/                      
        FIND FIRST EntErrorCd WHERE
                   EntErrorCd.SystemCode = "CMIPolicy"
             AND   EntErrorCd.ErrorField = nv_Cpolicy
        NO-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE EntErrorCd THEN
           ASSIGN EntS7072Result.ErrorCode    = EntErrorCd.ErrorCode
                  EntS7072Result.ErrorMessage = EntErrorCd.ErrorMessage .
      END.
/*[BLANK]*/                      
      IF nv_CBarCodeNumber <> "" THEN DO:
/*[BLANK]*/                      
        FIND FIRST EntErrorCd WHERE
                   EntErrorCd.SystemCode = "CMIPolicy"
             AND   EntErrorCd.ErrorField = nv_CBarCodeNumber
        NO-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE EntErrorCd THEN
           ASSIGN EntS7072Result.ErrorCode    = EntErrorCd.ErrorCode
                  EntS7072Result.ErrorMessage = EntErrorCd.ErrorMessage .
      END.
/*[BLANK]*/                      
      IF EntS7072Result.ErrorMessage = "" THEN EntS7072Result.ErrorMessage = nv_SAVEERROR.
/*[BLANK]*/                      
      ASSIGN
      EntS7072Result.SystemRq        = IntS7072.SystemRq
      EntS7072Result.CompanyCode     = IntS7072.CompanyCode
      EntS7072Result.InsurerId       = IntS7072.InsurerId
      EntS7072Result.ContractNumber  = IntS7072.ContractNumber
      EntS7072Result.RqUID           = IntS7072.RqUID
      EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq.
    END.
/*[BLANK]*/                      
    FIND FIRST EntS7072 WHERE
               EntS7072.SystemRq        = IntS7072.SystemRq
           AND EntS7072.CompanyCode     = IntS7072.CompanyCode
           AND EntS7072.Username        = IntS7072.Username
           AND EntS7072.Password        = IntS7072.Password
           AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
    NO-ERROR NO-WAIT.
    IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "X" .
/*[BLANK]*/                      
    IntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
  END. /*IF nv_AddData = NO THEN DO:*/
/*[COMMENT]*/                      /* ----------------------------------------------------------- */
/*[BLANK]*/                      
  OUTPUT TO PUT_IntS7072.TXT.
  PUT "                  |" TODAY FORMAT "99/99/9999" 
      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
  PUT "MethodCode = SPLIT|" IntS7072.MethodCode  SKIP.
  PUT "3  CompanyCode    |" IntS7072.CompanyCode SKIP.
  PUT "3  ContractNumber |" IntS7072.ContractNumber FORMAT "x(30)" SKIP.
  PUT "5  PolicyNumber   |" nv_PolicyV70 FORMAT "x(20)" SKIP.
  PUT "6  BarCodeNumber  |" IntS7072.StickerNumber SKIP.
  PUT "7  nv_DocnoV70    |" nv_DocnoV70  FORMAT "X(20)" SKIP.
  PUT "8  EffectiveDt    |" IntS7072.EffectiveDt SKIP.
  PUT "9  PlateNumber    |" IntS7072.PlateNumber SKIP.
  PUT "10 VehicleTypeCode|" IntS7072.VehicleTypeCode SKIP.
  PUT "24 InsuredName    |" IntS7072.InsuredName     SKIP.
  PUT "25 InsuredSurname |" IntS7072.InsuredSurname  SKIP.
  PUT "30 PolicyTypeCd   |" IntS7072.PolicyTypeCd FORMAT "x(5)" SKIP.
  PUT "31 RateGroup      |" IntS7072.RateGroup  SKIP.
  PUT "27 WrittenAmt     |" IntS7072.WrittenAmt SKIP.
  PUT "5  CMIPolicyNumber|" nv_PolicyV72 FORMAT "x(20)" SKIP.
  PUT "7  nv_DocnoV72    |" nv_DocnoV72  FORMAT "X(20)" SKIP.
  PUT "32 CMIPolicyTypeCd|" IntS7072.CMIPolicyTypeCd SKIP.
  PUT "33 CMIVehTypeCd   |" IntS7072.CMIVehTypeCd SKIP.
  PUT  FILL("-",78) FORMAT "X(78)" SKIP(1).
/*[BLANK]*/                      
  PUT "FileNameAttach1|" IntS7072.FileNameAttach1 SKIP.
  PUT "FileNameAttach2|" IntS7072.FileNameAttach2 SKIP.
  PUT "FileNameAttach3|" IntS7072.FileNameAttach3 SKIP.
/*[BLANK]*/                      
  PUT  FILL("-",78) FORMAT "X(78)" SKIP(1).
  OUTPUT  CLOSE.
END. /*IF AVAILABLE IntS7072 THEN DO: */
/*[BLANK]*/                      
/*[BLANK]*/                      
RELEASE EntS7072Result.
RELEASE EntS7072.
/*[BLANK]*/                      
RELEASE IntS7072.
RELEASE IntPol7072Result.
RELEASE IntPol7072.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkBlankForm C-Win 
PROCEDURE PD_ChkBlankForm :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_PrgName       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[BLANK]*/                      
    IF IntPol7072.CompanyCode = "833" THEN DO:
/*[BLANK]*/                          
      IF (IntPol7072.CMIDocumentUID   >= "7545177"       AND IntPol7072.CMIDocumentUID   <= "7545276")
      OR (IntPol7072.CMIDocumentUID   >= "7549101"       AND IntPol7072.CMIDocumentUID   <= "7549171")
      OR (IntPol7072.CMIDocumentUID   >= "7842501"       AND IntPol7072.CMIDocumentUID   <= "7845000")
      OR (IntPol7072.CMIDocumentUID   >= "7840001"       AND IntPol7072.CMIDocumentUID   <= "7842500")
      OR (IntPol7072.CMIDocumentUID   >= "7845001"       AND IntPol7072.CMIDocumentUID   <= "7845500")
/*[COMMENT]*/                          /* TEST
/*[COMMENT]*/                          OR (IntPol7072.CMIDocumentUID   >= "8330201"       AND IntPol7072.CMIDocumentUID   <= "8330300") */
      OR (IntPol7072.CMIBarCodeNumber >= "0210021101776" AND IntPol7072.CMIBarCodeNumber <= "0210021102760")
      OR (IntPol7072.CMIBarCodeNumber >= "0210021141013" AND IntPol7072.CMIBarCodeNumber <= "0210021141713")
      OR (IntPol7072.CMIBarCodeNumber >= "0210022425012" AND IntPol7072.CMIBarCodeNumber <= "0210022450002")
      OR (IntPol7072.CMIBarCodeNumber >= "0210022400011" AND IntPol7072.CMIBarCodeNumber <= "0210022425001")
      OR (IntPol7072.CMIBarCodeNumber >= "0210022450013" AND IntPol7072.CMIBarCodeNumber <= "0210022455005")
/*[COMMENT]*/                          /* TEST
/*[COMMENT]*/                          OR (IntPol7072.CMIBarCodeNumber >= "3595154544881" AND IntPol7072.CMIBarCodeNumber <= "3695185266662") */
      THEN DO:
/*[COMMENT]*/                            /*MESSAGE "OLD " SKIP(1)
/*[COMMENT]*/                                      IntPol7072.CMIDocumentUID SKIP
/*[COMMENT]*/                                      IntPol7072.CMIBarCodeNumber SKIP (1) 
/*[COMMENT]*/                              VIEW-AS ALERT-BOX.*/
      END.
      ELSE DO:
/*[COMMENT]*/                            /*Blank form Compulsory */
/*[BLANK]*/                      
/*[COMMENT]*/                            /*MESSAGE "NEW " SKIP(1)
/*[COMMENT]*/                                      IntPol7072.CMIDocumentUID SKIP
/*[COMMENT]*/                                      IntPol7072.CMIBarCodeNumber SKIP (1) 
/*[COMMENT]*/                              VIEW-AS ALERT-BOX.*/
/*[BLANK]*/                      
        FIND FIRST FNameAttach WHERE
                   FNameAttach.CompanyCode  = TRIM(IntPol7072.CompanyCode) + "_2"
               AND FNameAttach.PolicyTypeCd = "V72"
               AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
               AND FNameAttach.EffDate     <= TODAY
               AND FNameAttach.SelectNumber = 1
        NO-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE FNameAttach THEN
          ASSIGN
          nv_NameCompCd = FNameAttach.CompanyCode /*833_2*/
          nv_PrgName    = FNameAttach.PrgName
          nv_PrmPrg     = FNameAttach.PrmPrg. /*cmipolicy2*/
      END.
    END.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt1 C-Win 
PROCEDURE PD_ChkDataExt1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_Mdocno1 AS CHARACTER NO-UNDO. /*M*/
DEFINE INPUT PARAMETER nv_Tdocno1 AS CHARACTER NO-UNDO. /*T*/
/*[BLANK]*/                      
DEFINE VARIABLE nv_PolicyTypeCd AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE nv_SAVEError    AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
CREATE IntS7072.
/*[BLANK]*/                      
ASSIGN
IntS7072.Username             = EntS7072.Username
IntS7072.Password             = EntS7072.Password
IntS7072.CompanyCode          = EntS7072.CompanyCode
IntS7072.BranchCd             = EntS7072.BranchCd 
IntS7072.InsurerId            = EntS7072.InsurerId
IntS7072.InsuranceCd          = EntS7072.InsuranceCd
IntS7072.InsuranceBranchCd    = EntS7072.InsuranceBranchCd
IntS7072.PolicyStatus         = EntS7072.PolicyStatus
IntS7072.ContractNumber       = EntS7072.ContractNumber
IntS7072.ContractDt           = EntS7072.ContractDt
IntS7072.ContractTime         = EntS7072.ContractTime
IntS7072.CMVApplicationNumber = EntS7072.CMVApplicationNumber
IntS7072.ApplicationNumber    = EntS7072.ApplicationNumber  /* QNumPremium */
IntS7072.ApplicationDt        = EntS7072.ApplicationDt
IntS7072.ApplicationTime      = EntS7072.ApplicationTime
/*[BLANK]*/                      
IntS7072.InsuredType                 = EntS7072.InsuredType
IntS7072.InsuredUniqueID             = REPLACE(EntS7072.InsuredUniqueID,CHR(13),"") 
IntS7072.InsuredUniqueIDExpDt        = EntS7072.InsuredUniqueIDExpDt
IntS7072.InsuredUniqueIDExpirationDt = EntS7072.InsuredUniqueIDExpirationDt
IntS7072.License        = EntS7072.License
IntS7072.BirthDt        = EntS7072.BirthDt
IntS7072.InsuredCd      = EntS7072.InsuredCd
IntS7072.InsuredTitle   = EntS7072.InsuredTitle
/*[COMMENT]*/                        /*
/*[COMMENT]*/                    IntS7072.InsuredName    = EntS7072.InsuredName                        /*Add by Kridtiya i. Date.02/02/2017*/
IntS7072.InsuredSurname = EntS7072.InsuredSurname*/                   /*Add by Kridtiya i. Date.02/02/2017*/
IntS7072.InsuredName    = replace(EntS7072.InsuredName,CHR(26),"")    /*Add by Kridtiya i. Date.02/02/2017*/
IntS7072.InsuredSurname = replace(EntS7072.InsuredSurname,CHR(26),"") /*Add by Kridtiya i. Date.02/02/2017*/
IntS7072.InsuredBranch  = EntS7072.InsuredBranch
IntS7072.Addr           = EntS7072.Addr
IntS7072.UnitNumber     = EntS7072.UnitNumber
IntS7072.RoomNumber     = EntS7072.RoomNumber
IntS7072.Building       = EntS7072.Building
IntS7072.VillageNumber  = EntS7072.VillageNumber
IntS7072.Alley          = EntS7072.Alley
IntS7072.Lane           = EntS7072.Lane
IntS7072.StreetName     = EntS7072.StreetName
IntS7072.SubDistrict    = EntS7072.SubDistrict
IntS7072.District       = EntS7072.District
IntS7072.StateProvCd    = EntS7072.StateProvCd
IntS7072.StateProv      = EntS7072.StateProv
IntS7072.Province       = EntS7072.Province
IntS7072.PostalCode     = EntS7072.PostalCode
IntS7072.OccupationDesc = EntS7072.OccupationDesc
/*[BLANK]*/                      
IntS7072.MobilePhoneNumber = EntS7072.MobilePhoneNumber
IntS7072.MobileNumber      = EntS7072.MobileNumber
IntS7072.PhoneNumber       = EntS7072.PhoneNumber
IntS7072.OfficePhoneNumber = EntS7072.OfficePhoneNumber
IntS7072.EmailAddr         = EntS7072.EmailAddr
/*[BLANK]*/                      
IntS7072.ReceiptName = EntS7072.ReceiptName
IntS7072.ReceiptAddr = EntS7072.ReceiptAddr
/*[BLANK]*/                      
IntS7072.DriverNameCd  = EntS7072.DriverNameCd
IntS7072.InsuredTitle1 = EntS7072.InsuredTitle1
IntS7072.InsuredName1  = EntS7072.InsuredName1
IntS7072.InsuredSurname1 = EntS7072.InsuredSurname1
IntS7072.OccupationDesc1 = EntS7072.OccupationDesc1
IntS7072.BirthDt1 = EntS7072.BirthDt1
IntS7072.InsuredUniqueID1 = EntS7072.InsuredUniqueID1
IntS7072.License1 = EntS7072.License1
IntS7072.InsuredTitle2 = EntS7072.InsuredTitle2
IntS7072.InsuredName2  = EntS7072.InsuredName2
IntS7072.InsuredSurname2 = EntS7072.InsuredSurname2
IntS7072.OccupationDesc2 = EntS7072.OccupationDesc2
IntS7072.BirthDt2 = EntS7072.BirthDt2
IntS7072.InsuredUniqueID2 = EntS7072.InsuredUniqueID2
IntS7072.License2  = EntS7072.License2
/*[BLANK]*/                      
IntS7072.Beneficiaries = EntS7072.Beneficiaries
IntS7072.PolicyAttachment = EntS7072.PolicyAttachment
IntS7072.VehicleUse = EntS7072.VehicleUse
IntS7072.PromptText = EntS7072.PromptText
/*[BLANK]*/                      
IntS7072.VehGroup        = EntS7072.VehGroup
IntS7072.VehTypeCd       = EntS7072.VehTypeCd
IntS7072.VehicleTypeCode = EntS7072.VehicleTypeCode
IntS7072.Manufacturer    = REPLACE(EntS7072.Manufacturer,   CHR(13),"")  
IntS7072.Model           = REPLACE(EntS7072.Model,          CHR(13),"") 
IntS7072.ModelTypeName   = EntS7072.ModelTypeName           
IntS7072.ModelYear       = EntS7072.ModelYear               
IntS7072.VehBodyTypeDesc = REPLACE(EntS7072.VehBodyTypeDesc,CHR(13),"") 
IntS7072.SeatingCapacity = EntS7072.SeatingCapacity
IntS7072.Displacement    = EntS7072.Displacement
/*[BLANK]*/                      
IntS7072.GrossVehOrCombinedWeight = EntS7072.GrossVehOrCombinedWeight
IntS7072.ColourCd = EntS7072.ColourCd
IntS7072.Colour   = EntS7072.Colour
/*[BLANK]*/                      
IntS7072.ChassisVINNumber    = EntS7072.ChassisVINNumber
IntS7072.ChassisSerialNumber = EntS7072.ChassisSerialNumber
IntS7072.EngineSerialNumber  = EntS7072.EngineSerialNumber
/*[BLANK]*/                      
IntS7072.Registration           = EntS7072.Registration
IntS7072.RegisteredProvCd       = EntS7072.RegisteredProvCd
IntS7072.PlateNumber            = EntS7072.PlateNumber
IntS7072.RegisteredProvinceCode = EntS7072.RegisteredProvinceCode
/*[BLANK]*/                      
IntS7072.RegisteredYear = EntS7072.RegisteredYear
/*[BLANK]*/                      
IntS7072.SumInsureAmt = EntS7072.SumInsureAmt .
/*[BLANK]*/                      
ASSIGN
IntS7072.PolicyTypeCd = EntS7072.PolicyTypeCd
IntS7072.RateGroup    = EntS7072.RateGroup
/*[BLANK]*/                      
IntS7072.PolicyNumber = EntS7072.PolicyNumber
IntS7072.PreviousPolicyNumber = EntS7072.PreviousPolicyNumber
/*[COMMENT]*/                    /*IntS7072.DocumentUID  = EntS7072.DocumentUID */ /*kridtiya i. 03/02/2020*/
/*[BLANK]*/                      
IntS7072.AgreeDt = EntS7072.AgreeDt
IntS7072.IssueDt = EntS7072.IssueDt
IntS7072.EffectiveDt  = EntS7072.EffectiveDt
IntS7072.ExpirationDt = EntS7072.ExpirationDt
IntS7072.EndDt   = EntS7072.EndDt
IntS7072.SetTime = EntS7072.SetTime
/*[BLANK]*/                      
IntS7072.TPBIAmtPerson   = EntS7072.TPBIAmtPerson
IntS7072.TPBIAmtAccident = EntS7072.TPBIAmtAccident
IntS7072.PDAmtAccident   = EntS7072.PDAmtAccident
IntS7072.DeductiblePDAmtAccident = EntS7072.DeductiblePDAmtAccident
IntS7072.COLLAmtAccident           = EntS7072.COLLAmtAccident
IntS7072.DeductibleCOLLAmtAccident = EntS7072.DeductibleCOLLAmtAccident
/*[BLANK]*/                      
IntS7072.FTAmt = EntS7072.FTAmt
/*[BLANK]*/                      
IntS7072.PerilsPADriverAmt = EntS7072.PerilsPADriverAmt
IntS7072.PerilsPANumPassengers = EntS7072.PerilsPANumPassengers
IntS7072.PerilsPAPassengerAmt  = EntS7072.PerilsPAPassengerAmt
IntS7072.PerilsPATemporaryDriverAmt  = EntS7072.PerilsPATemporaryDriverAmt
IntS7072.PerilsPANumTemporaryPassengers = EntS7072.PerilsPANumTemporaryPassengers
IntS7072.PerilsPATemporaryPassengerAmt  = EntS7072.PerilsPATemporaryPassengerAmt
IntS7072.PerilsMedicalTreatmentAmt  = EntS7072.PerilsMedicalTreatmentAmt
IntS7072.PerilsBailBondInsuranceAmt = EntS7072.PerilsBailBondInsuranceAmt
/*[BLANK]*/                      
IntS7072.DiscountForNamedDriver = EntS7072.DiscountForNamedDriver
IntS7072.DeductibleAmt = EntS7072.DeductibleAmt
IntS7072.FleetAmt      = EntS7072.FleetAmt
IntS7072.GoodDriverIndPct = EntS7072.GoodDriverIndPct
IntS7072.GoodDriverIndAmt = EntS7072.GoodDriverIndAmt
IntS7072.OtherDiscountAmt = EntS7072.OtherDiscountAmt
IntS7072.TotalDiscountsAmt  = EntS7072.TotalDiscountsAmt
IntS7072.SurchargeFactorAmt = EntS7072.SurchargeFactorAmt
IntS7072.PremiumCoverage13Amt = EntS7072.PremiumCoverage13Amt
IntS7072.PremiumCoverage2Amt  = EntS7072.PremiumCoverage2Amt
/*[BLANK]*/                      
IntS7072.ReceiptNumber   = EntS7072.ReceiptNumber /* CampaignNumber / PromotionNumber */
IntS7072.WrittenAmt      = EntS7072.WrittenAmt
IntS7072.RevenueStampAmt = EntS7072.RevenueStampAmt
IntS7072.VatAmt          = EntS7072.VatAmt
IntS7072.CurrentTermAmt  = EntS7072.CurrentTermAmt
/*[BLANK]*/                      
IntS7072.GarageTypeCd    = EntS7072.GarageTypeCd
IntS7072.GarageDesc      = EntS7072.GarageDesc
IntS7072.OptionValueDesc = EntS7072.OptionValueDesc .
/*[BLANK]*/                      
ASSIGN
IntS7072.CMIPolicyTypeCd = EntS7072.CMIPolicyTypeCd 
IntS7072.CMIVehTypeCd    = EntS7072.CMIVehTypeCd
IntS7072.CMIPolicyNumber = EntS7072.CMIPolicyNumber
IntS7072.CMIApplicationNumber = EntS7072.CMIApplicationNumber
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    IntS7072.CMIBarCodeNumber = EntS7072.CMIBarCodeNumber
IntS7072.CMIDocumentUID   = EntS7072.CMIDocumentUID*/  /*kridtiya i. 03/02/2020*/
/*[BLANK]*/                      
IntS7072.CMIEffectiveDt  = EntS7072.CMIEffectiveDt
IntS7072.CMIExpirationDt = EntS7072.CMIExpirationDt
IntS7072.CMIAmtPerson    = EntS7072.CMIAmtPerson
IntS7072.CMIAmtAccident  = EntS7072.CMIAmtAccident
IntS7072.CMIWrittenAmt   = EntS7072.CMIWrittenAmt
IntS7072.CMIRevenueStampAmt = EntS7072.CMIRevenueStampAmt
IntS7072.CMIVatAmt          = EntS7072.CMIVatAmt
IntS7072.CMICurrentTermAmt  = EntS7072.CMICurrentTermAmt
/*[BLANK]*/                      
IntS7072.MsgStatusCd    = EntS7072.MsgStatusCd
IntS7072.AgencyEmployee = EntS7072.AgencyEmployee 
IntS7072.RemarkText     = EntS7072.RemarkText
/*[BLANK]*/                      
IntS7072.SystemRq = EntS7072.SystemRq
IntS7072.RqUID    = EntS7072.RqUID
IntS7072.keyRequestIndRq = EntS7072.keyRequestIndRq
IntS7072.Policy = EntS7072.Policy
IntS7072.Rencnt = EntS7072.Rencnt
IntS7072.Endcnt = EntS7072.Endcnt
IntS7072.Riskno = EntS7072.Riskno
IntS7072.Itemno = EntS7072.Itemno
IntS7072.vehreg = EntS7072.vehreg
IntS7072.accdat = EntS7072.accdat
IntS7072.comdat = EntS7072.comdat
IntS7072.expdat = EntS7072.expdat
IntS7072.CMIComDate    = EntS7072.CMIComDate
IntS7072.CMIExpDate    = EntS7072.CMIExpDate
IntS7072.BirthDate     = EntS7072.BirthDate
IntS7072.InsUIDExpDate = EntS7072.InsUIDExpDate
/*[BLANK]*/                      
IntS7072.ReceiptName2 = EntS7072.ReceiptName2
IntS7072.ReceiptAddr2 = EntS7072.ReceiptAddr2
IntS7072.TransactionDateRq = EntS7072.TransactionDateRq
IntS7072.TransactionTimeRq = EntS7072.TransactionTimeRq
IntS7072.BirthDate1 = EntS7072.BirthDate1
IntS7072.BirthDate2 = EntS7072.BirthDate2
IntS7072.InsUIDExpDate1 = EntS7072.InsUIDExpDate1
IntS7072.InsUIDExpDate2 = EntS7072.InsUIDExpDate2
IntS7072.RegisterDt = EntS7072.RegisterDt
IntS7072.ShowroomID = EntS7072.ShowroomID
IntS7072.ShowroomName = EntS7072.ShowroomName
IntS7072.ReceiptDt  = EntS7072.ReceiptDt
/*[BLANK]*/                      
IntS7072.AgentBrokerLicenseNumber = nv_Acno1 /*EntS7072.AgentBrokerLicenseNumber*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /*02/10/2015*/
IntS7072.Statusflag = EntS7072.Statusflag  /*Flag Ã¶·Ñ¹ã¨ à©¾ÒÐ»ÃÐàÀ· 1*/
IntS7072.Finint     = EntS7072.Finint /*Deler / VAT CODE*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IntS7072.TransferToPremium = NO .
/*[COMMENT]*/                    /* --------------------------------------- */
/*[BLANK]*/                      
RUN PD_ChkDataExt2. /*Save ¢éÍÁÙÅÊèÇ¹·ÕèàËÅ×Í*/
RUN PD_ChkDataExt3. /*Save ¢éÍÁÙÅÊèÇ¹·ÕèàËÅ×Í*/
/*[COMMENT]*/                    /* --------------------------------------- */
/*[BLANK]*/                      
IF IntS7072.PolicyStatus   = "" THEN IntS7072.PolicyStatus   = "N".
IF IntS7072.DocumentUID    = "" THEN IntS7072.DocumentUID    = nv_Mdocno1.
IF IntS7072.CMIDocumentUID = "" THEN IntS7072.CMIDocumentUID = nv_Tdocno1.
/*[BLANK]*/                      
IF IntS7072.CMIVehTypeCd <> "" THEN DO:
  FIND FIRST FMSclassCST WHERE FMSclassCST.Sclass = IntS7072.CMIVehTypeCd
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE FMSclassCST THEN DO:
    IF FMSclassCST.CST = "C" THEN DO:
      IF IntS7072.Displacement = "" THEN DO:
        IF FMSclassCST.MaxCST <> 0 THEN 
          IntS7072.Displacement = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
      END.
    END.
    IF FMSclassCST.CST = "S" THEN DO:
      IF IntS7072.SeatingCapacity = "" THEN DO:
        IF FMSclassCST.MaxCST <> 0 THEN 
          IntS7072.SeatingCapacity = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
      END.
    END.
    IF FMSclassCST.CST = "T" THEN DO:
      IF IntS7072.GrossVehOrCombinedWeight = "" THEN DO:
/*[BLANK]*/                      
        IF FMSclassCST.MaxCST <> 0 THEN 
          IntS7072.GrossVehOrCombinedWeight = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
      END.
    END.
  END.
END.
/*[COMMENT]*/                    /**/
IF LENGTH(TRIM(IntS7072.RegisteredProvCd)) > 2 THEN DO:
  FOR EACH uwm500 USE-INDEX uwm50002 NO-LOCK:
    IF INDEX(uwm500.prov_d,IntS7072.RegisteredProvCd) <> 0 THEN DO:
      IntS7072.RegisteredProvCd = uwm500.prov_n.
      LEAVE.
    END.
    IF CAN-DO(uwm500.prov_d,IntS7072.RegisteredProvCd) THEN DO:
      IntS7072.RegisteredProvCd = uwm500.prov_n.
      LEAVE.
    END.
  END.
END.
IntS7072.RegisteredProvinceCode = IntS7072.RegisteredProvCd.
IF EntS7072.CompanyCode = "469" THEN IntS7072.vehreg = TRIM(IntS7072.Registration). /*kridtiyai.*/
ELSE IntS7072.vehreg = TRIM(IntS7072.Registration) + " " + TRIM(IntS7072.RegisteredProvCd).
/*[COMMENT]*/                    /**/
ASSIGN
IntS7072.ResultStatus     = ""   /*EntS7072.ResultStatus*/
IntS7072.ProcessStatus    = "O"
IntS7072.ProcessByUser    = EntS7072.ProcessByUser /*"Queue"*/
IntS7072.ProcessDate      = TODAY
IntS7072.ProcessTime      = STRING(TIME,"HH:MM:SS")
IntS7072.TrnFromIntByUser = EntS7072.TrnFromIntByUser /*"Queue"*/
IntS7072.TrnFromIntDate   = TODAY
IntS7072.TrnFromIntTime   = STRING(TIME,"HH:MM:SS")
/*[COMMENT]*/                    /*yyyymmddhhmmss999*/
IntS7072.ReferenceNumber  = STRING( YEAR(TODAY),"9999")
                          + STRING(MONTH(TODAY),"99")
                          + STRING(  DAY(TODAY),"99")
                          + SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12).
IntS7072.ReferenceNumber  = REPLACE(IntS7072.ReferenceNumber,":","").
IntS7072.ReferenceNumber  = REPLACE(IntS7072.ReferenceNumber,".","").
/*[BLANK]*/                      
ASSIGN
IntS7072.ReceiveNumber = IntS7072.ReferenceNumber
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÃÐºØµÒÁ USER ID ·ÕèConfirm data ¡èÍ¹Êè§¡ÅÑº ä»ãËé REQUESTOR */
IntS7072.ConfirmBy     = nv_ConfirmBy
/*[BLANK]*/                      
EntS7072.ProcessStatus = "O".
/*[COMMENT]*/                    /* --------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*µÃÇ¨ÊÍº¢éÍÁÙÅ â´Âà¨éÒË¹éÒ·Õè »ÃÐàÀ· 1,2,2.1,3,3.1*/
nv_PolicyTypeCd = "".
IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN
  nv_PolicyTypeCd = IntS7072.PolicyTypeCd.
ELSE DO:
  IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
    nv_PolicyTypeCd = IntS7072.CMIPolicyTypeCd.
    FOR EACH msgcode WHERE
             MsgCode.CompNo = IntS7072.CompanyCode
         AND MsgCode.MsgNo  = "GrpClass"
    NO-LOCK:
      IF msgcode.Branch = IntS7072.CMIVehTypeCd AND MsgCode.MsgDesc = "NOTSALE" THEN DO:
        IntS7072.ProcessStatus = "E".
        EntS7072.ProcessStatus = "E".
        RUN PD_SaveError ("äÁè¾ºÃËÑÊ¾Ãº. " + IntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ").
        RETURN.
      END.
    END.
  END.
END.
FIND FIRST FModify WHERE
           FModify.SystemName  = IntS7072.SystemRq
     AND   FModify.CompanyCode = IntS7072.CompanyCode
     AND   FModify.CoverTypeCd = nv_PolicyTypeCd 
NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE FModify THEN DO:
/*[BLANK]*/                      
  IF FModify.AutoAll = YES THEN DO:   /*ãËé¼èÒ¹ä´é·Ñé§ËÁ´*/
    ASSIGN
    IntS7072.ConfirmBy        = "AUTO"
    IntS7072.ByUserID         = FModify.ChkVehAssignBy
    IntS7072.ChkVehicle       = NO
    IntS7072.ChkVehAssignBy   = FModify.ChkVehAssignBy
    IntS7072.ChkVehAssignDt   = TODAY
    IntS7072.ChkVehAssignTime = STRING(TIME,"HH:MM:SS").
  END.
  ELSE DO:
/*[COMMENT]*/                        /*µÃÇ¨ÊÀÒ¾Ã¶ â´Âà¨éÒË¹éÒ·Õè »ÃÐàÀ· 1,2,2.1*/
    IF FModify.ChkVehicle = NO THEN 
      ASSIGN
      IntS7072.ConfirmBy  = "AUTO" /*äÁèµéÍ§µÃÇ¨ÊÀÒ¾*/
      IntS7072.ChkVehicle = NO.
    ELSE
      ASSIGN
      IntS7072.ConfirmBy  = "NO"               /*Êè§ µÃÇ¨ÊÀÒ¾Ã¶*/
      IntS7072.ChkVehicle = FModify.ChkVehicle /*YES*/
      IntS7072.ChkVehDt   = TODAY
      IntS7072.ChkVehTime = STRING(TIME,"HH:MM:SS")
      IntS7072.ChkVehBy   = FModify.AssignTO         /*ª×èÍà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÀÒ¾Ã¶*/
      IntS7072.ChkVehSend = FModify.AssignSendToMail /*Y/N Êè§mailãËéà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÍºÊÀÒ¾Ã¶ËÃ×ÍäÁè */
      IntS7072.ChkVehMail = FModify.AssignSendMail   /*mail à¨éÒË¹éÒ·ÕèµÃÇ¨ÊÀÒ¾Ã¶*/
      IntS7072.ChkVehAssignSend = FModify.ChkVehAssignSendToMail /*Y/N Êè§mailËÃ×ÍäÁè */
      IntS7072.ChkVehAssignMail = FModify.ChkVehAssignMail       /*mail á¨é§ user µÃÇ¨ÊÍº¢éÍÁÙÅ*/
/*[COMMENT]*/                          /* »ÃÐàÀ· 1,2,2.1 */
/*[COMMENT]*/                          /* FModify.TransferToPremium = YES ãËéâÍ¹¢éÍÁÙÅà¢éÒÃÐºº Premium*/
/*[COMMENT]*/                          /* ÊÓËÃÑºà»ç¹¢éÍÁÙÅã¹¡ÒÃÊè§µÃÇ¨ÊÀÒ¾Ã¶ áÅÐ¶éÒ¼èÒ¹ÊÒÁÒÃ¶·Ó§Ò¹ä´é·Ñ¹·Õ*/
      IntS7072.TransferToPremium = FModify.TransferToPremium . 
/*[COMMENT]*/                        /* -------------------------------------------------- */
    ASSIGN
    IntS7072.ByUserID          = FModify.ChkVehAssignBy
    IntS7072.ChkVehAssignBy    = FModify.ChkVehAssignBy /*ª×èÍà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÍº¢éÍÁÙÅ*/
    IntS7072.ChkVehAssignDt    = TODAY
    IntS7072.ChkVehAssignTime  = STRING(TIME,"HH:MM:SS").
  END.
/*[BLANK]*/                      
  OUTPUT TO ChkVeh_PD_Ext1.txt APPEND.
  PUT "PD_ChkDataExt1 : " TODAY FORMAT "99/99/9999" STRING(TIME,"HH:MM:SS") SKIP.
  PUT "-----------------------------------------------------------" FORMAT "x(80)" SKIP.
  PUT "ContractNumber:" IntS7072.ContractNumber SKIP.
  PUT "PolicyTypeCd:" IntS7072.PolicyTypeCd FORMAT "X(5)" SKIP. 
  PUT "RateGroup  :" IntS7072.RateGroup   SKIP.
  PUT "DocumentUID:" IntS7072.DocumentUID SKIP. 
  PUT "EntS7072.RegisteredProvCd:" EntS7072.RegisteredProvCd FORMAT "X(15)" SKIP.
  PUT "IntS7072.RegisteredProvCd:" IntS7072.RegisteredProvCd FORMAT "X(15)" SKIP.
  PUT "Registration :" IntS7072.Registration FORMAT "X(15)" SKIP.
  PUT "CMIPolicyTypeCd:" IntS7072.CMIPolicyTypeCd FORMAT "X(5)" SKIP.
  PUT "CMIVehTypeCd  :" IntS7072.CMIVehTypeCd   SKIP.
  PUT "CMIDocumentUID:" IntS7072.CMIDocumentUID SKIP.
  PUT "ConfirmBy :" IntS7072.ConfirmBy  SKIP.
  PUT "ByUserID  :" IntS7072.ByUserID   SKIP.
  PUT "ChkVehicle:" IntS7072.ChkVehicle SKIP.
  PUT "ChkVehBy  :" IntS7072.ChkVehBy   SKIP.
  PUT "ChkVehDt  :" IntS7072.ChkVehDt   SKIP.
  PUT "ChkVehTime:" IntS7072.ChkVehTime SKIP.
  PUT "ChkVehSend:" IntS7072.ChkVehSend SKIP.
  PUT "ChkVehMail:" IntS7072.ChkVehMail SKIP.
  PUT "ChkVehAssignBy:" IntS7072.ChkVehAssignBy SKIP.
  PUT "ChkVehAssignDt:" IntS7072.ChkVehAssignDt SKIP.
  PUT "ChkVehAssignTime :" IntS7072.ChkVehAssignTime SKIP.
  PUT "TransferToPremium:" IntS7072.TransferToPremium SKIP.
  OUTPUT CLOSE.
END.
ELSE DO:
  ASSIGN
  IntS7072.ConfirmBy = "AUTO"
  IntS7072.ByUserID  = "AUTObyApplication".
END.
RELEASE IntS7072.
RELEASE EntS7072.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt2 C-Win 
PROCEDURE PD_ChkDataExt2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
ASSIGN
IntS7072.ResponseResult     = EntS7072.ResponseResult
/*[BLANK]*/                      
IntS7072.EndorseEffectiveDt = EntS7072.EndorseEffectiveDt
IntS7072.EndorseRefNumber   = EntS7072.EndorseRefNumber
IntS7072.EndorseFlag        = EntS7072.EndorseFlag
/*[BLANK]*/                      
IntS7072.InsurerCode        = EntS7072.InsurerCode
IntS7072.MethodCode         = EntS7072.MethodCode
/*[BLANK]*/                      
IntS7072.SendByUser         = EntS7072.SendByUser
IntS7072.SendDate           = EntS7072.SendDate
IntS7072.SendTime           = EntS7072.SendTime
/*[BLANK]*/                      
IntS7072.SystemErrorStatus1 = EntS7072.SystemErrorStatus1
IntS7072.SystemErrorStatus2 = EntS7072.SystemErrorStatus2
IntS7072.SystemErrorStatus3 = EntS7072.SystemErrorStatus3
/*[BLANK]*/                      
IntS7072.EndorseReceiveNumber = EntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
IntS7072.ErrorMessage         = EntS7072.ErrorMessage
IntS7072.ByUserID             = EntS7072.ByUserID
IntS7072.StickerNumber        = EntS7072.StickerNumber.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd," ","").
IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd,".","").
IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd,"-","").
/*[BLANK]*/                      
IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(13),"").
IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(13),"").
IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(13),"").
IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(13),"").
IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(13),"").
IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(13),"").
IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(13),"").
IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(13),"").
IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(13),"").
IntS7072.District      = REPLACE(IntS7072.District,     CHR(13),"").
IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(13),"").
IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(13),"").
/*[BLANK]*/                      
IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(10),"").
IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(10),"").
IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(10),"").
IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(10),"").
IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(10),"").
IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(10),"").
IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(10),"").
IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(10),"").
IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(10),"").
IntS7072.District      = REPLACE(IntS7072.District,     CHR(10),"").
IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(10),"").
IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(10),"").
/*[COMMENT]*/                    /*add by kridtiya i. 03/04/2017*/
IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(26),"").
IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(26),"").
IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(26),"").
IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(26),"").
IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(26),"").
IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(26),"").
IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(26),"").
IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(26),"").
IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(26),"").
IntS7072.District      = REPLACE(IntS7072.District,     CHR(26),"").
IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(26),"").
IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(26),"").
/*[COMMENT]*/                    /*add by kridtiya i. 03/04/2017*/
IntS7072.ReceiptName   = REPLACE(IntS7072.ReceiptName,  CHR(13),"").
IntS7072.ReceiptAddr   = REPLACE(IntS7072.ReceiptAddr,  CHR(13),"").
IntS7072.Beneficiaries = REPLACE(IntS7072.Beneficiaries,CHR(13),"").
IntS7072.ReceiptName   = REPLACE(IntS7072.ReceiptName,  CHR(10),"").
IntS7072.ReceiptAddr   = REPLACE(IntS7072.ReceiptAddr,  CHR(10),"").
IntS7072.Beneficiaries = REPLACE(IntS7072.Beneficiaries,CHR(10),"").
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IntS7072.Registration  = REPLACE(IntS7072.Registration,"-"," ").
IntS7072.Registration  = REPLACE(IntS7072.Registration,"  "," ").
IntS7072.PlateNumber   = IntS7072.Registration.
/*[COMMENT]*/                    /**/
ASSIGN
IntS7072.COUNTER_NO                = EntS7072.COUNTER_NO
IntS7072.TERM_NO                   = EntS7072.TERM_NO
IntS7072.SERVICE_RUN_NO            = EntS7072.SERVICE_RUN_NO
IntS7072.RECORD_STATUS             = EntS7072.RECORD_STATUS
IntS7072.CLIENT_SERVICE_RUNNO      = EntS7072.CLIENT_SERVICE_RUNNO
IntS7072.ZONE                      = EntS7072.ZONE
IntS7072.R_SERVICE_RUNN            = EntS7072.R_SERVICE_RUNN
IntS7072.CANCEL_OPERATING          = EntS7072.CANCEL_OPERATING
IntS7072.OPERATE_BY_STAFF          = EntS7072.OPERATE_BY_STAFF
IntS7072.SYSTEM_DATE_TIME          = EntS7072.SYSTEM_DATE_TIME
IntS7072.USERID_CS                 = EntS7072.USERID_CS
IntS7072.PASSWORD_CS               = EntS7072.PASSWORD_CS
IntS7072.SUCCESS                   = EntS7072.SUCCESS
IntS7072.CODE                      = EntS7072.CODE
IntS7072.DESC_CS                   = EntS7072.DESC_CS
IntS7072.METHOD                    = EntS7072.METHOD
IntS7072.TX_ID                     = EntS7072.TX_ID
IntS7072.LOG_ID                    = EntS7072.LOG_ID
IntS7072.VENDOR_ID                 = EntS7072.VENDOR_ID
IntS7072.SERVICE_ID                = EntS7072.SERVICE_ID
IntS7072.INSURER_PRODUCT_LINE_CODE = EntS7072.INSURER_PRODUCT_LINE_CODE
IntS7072.PRODUCT_CODE              = EntS7072.PRODUCT_CODE
IntS7072.PLAN_CODE                 = EntS7072.PLAN_CODE
IntS7072.CATEGORY                  = EntS7072.CATEGORY
IntS7072.BILLING_FREQ              = EntS7072.BILLING_FREQ
IntS7072.NET_PREMIUM               = EntS7072.NET_PREMIUM
IntS7072.GROSS_PREMIUM             = EntS7072.GROSS_PREMIUM
IntS7072.SUM_INSURE                = EntS7072.SUM_INSURE
IntS7072.PRINT_SLIP                = EntS7072.PRINT_SLIP
IntS7072.POL_NO                    = EntS7072.POL_NO
IntS7072.CERT_NO                   = EntS7072.CERT_NO
IntS7072.OLD_POL_NO                = EntS7072.OLD_POL_NO
IntS7072.NEW_RENEW                 = EntS7072.NEW_RENEW
IntS7072.POL_YEAR_SEQ              = EntS7072.POL_YEAR_SEQ
IntS7072.SALE_DATE                 = EntS7072.SALE_DATE
IntS7072.EFFECTIVE_DATE            = EntS7072.EFFECTIVE_DATE
IntS7072.END_DATE                  = EntS7072.END_DATE
IntS7072.NID_NO                    = EntS7072.NID_NO
IntS7072.CARD_TITLE                = EntS7072.CARD_TITLE
IntS7072.CARD_NAME                 = EntS7072.CARD_NAME
IntS7072.CARD_MNAME                = EntS7072.CARD_MNAME
IntS7072.CARD_SNAME                = EntS7072.CARD_SNAME
IntS7072.CARD_DOB                  = EntS7072.CARD_DOB
IntS7072.CARD_GENDER               = EntS7072.CARD_GENDER
IntS7072.CARD_ADDRESS              = EntS7072.CARD_ADDRESS
IntS7072.CARD_SUB_DISTRICT         = EntS7072.CARD_SUB_DISTRICT
IntS7072.CARD_DISTRICT             = EntS7072.CARD_DISTRICT
IntS7072.CARD_PROVINCE             = EntS7072.CARD_PROVINCE
IntS7072.CARD_ISSUE_DATE           = EntS7072.CARD_ISSUE_DATE
IntS7072.CARD_EXPIRED_DATE         = EntS7072.CARD_EXPIRED_DATE
IntS7072.TEL_NO                    = EntS7072.TEL_NO
IntS7072.CURRENT_ADDRESS           = EntS7072.CURRENT_ADDRESS
IntS7072.CURRENT_SUB_DISTRICT      = EntS7072.CURRENT_SUB_DISTRICT
IntS7072.CURRENT_DISTRICT          = EntS7072.CURRENT_DISTRICT
IntS7072.CURRENT_PROVINCE          = EntS7072.CURRENT_PROVINCE
IntS7072.PAYCODE                   = EntS7072.PAYCODE
IntS7072.CHASSIS_NO                = EntS7072.CHASSIS_NO
IntS7072.VEHICLE_REG_DATE          = EntS7072.VEHICLE_REG_DATE
IntS7072.CAR_REG_NO                = EntS7072.CAR_REG_NO
IntS7072.CAR_REG_PROVINCE          = EntS7072.CAR_REG_PROVINCE
IntS7072.VEHICLE_TYPE_CODE         = EntS7072.VEHICLE_TYPE_CODE
IntS7072.VEHICLE_BODY_TYPE         = EntS7072.VEHICLE_BODY_TYPE
IntS7072.VEHICLE_MAKE              = EntS7072.VEHICLE_MAKE
IntS7072.VEHICLE_MODEL             = EntS7072.VEHICLE_MODEL
IntS7072.VEHICLE_USE_CODE          = EntS7072.VEHICLE_USE_CODE
IntS7072.ENGINE_CC                 = EntS7072.ENGINE_CC
IntS7072.USE_AREA                  = EntS7072.USE_AREA
IntS7072.DRIVER_TITLE_1            = EntS7072.DRIVER_TITLE_1
IntS7072.DRIVER_NAME_1             = EntS7072.DRIVER_NAME_1
IntS7072.DRIVER_SURNAME_1          = EntS7072.DRIVER_SURNAME_1
IntS7072.DRIVER_GENDER_1           = EntS7072.DRIVER_GENDER_1
IntS7072.DRIVER_AGE_1              = EntS7072.DRIVER_AGE_1
IntS7072.DRIVER_LICENSE_1          = EntS7072.DRIVER_LICENSE_1
IntS7072.DRIVER_TITLE_2            = EntS7072.DRIVER_TITLE_2
IntS7072.DRIVER_NMAE_2             = EntS7072.DRIVER_NMAE_2
IntS7072.DRIVER_SURNAME_2          = EntS7072.DRIVER_SURNAME_2
IntS7072.DRIVER_GENDER_2           = EntS7072.DRIVER_GENDER_2
IntS7072.DRIVER_AGE_2              = EntS7072.DRIVER_AGE_2
IntS7072.DRIVER_LICENSE_2          = EntS7072.DRIVER_LICENSE_2
IntS7072.COMP_BARCODE              = EntS7072.COMP_BARCODE
IntS7072.RECEIVE_NO                = EntS7072.RECEIVE_NO
IntS7072.INVOICE_NO                = EntS7072.INVOICE_NO
IntS7072.STAMP_RATE                = EntS7072.STAMP_RATE
IntS7072.VAT                       = EntS7072.VAT .
/*[COMMENT]*/                    /* ------ */
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt3 C-Win 
PROCEDURE PD_ChkDataExt3 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
IF LENGTH(TRIM(EntS7072.RegisteredProvCd)) > 2 THEN DO:
    IF      index(EntS7072.RegisteredProvCd,"¡·")              <> 0 THEN IntS7072.RegisteredProvCd = "¡·".  
    ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃØ§à·¾")         <> 0 THEN IntS7072.RegisteredProvCd = "¡·".
    ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃØ§à·¾ÁËÒ¹¤Ã")   <> 0 THEN IntS7072.RegisteredProvCd = "¡·".
    ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃÐºÕè")          <> 0 THEN IntS7072.RegisteredProvCd = "¡º".
    ELSE IF index(EntS7072.RegisteredProvCd,"¡Ò­¨¹ºØÃÕ")       <> 0 THEN IntS7072.RegisteredProvCd = "¡¨".
    ELSE IF index(EntS7072.RegisteredProvCd,"¡ÒÌÊÔ¹¸Øì")       <> 0 THEN IntS7072.RegisteredProvCd = "¡Ê".
    ELSE IF index(EntS7072.RegisteredProvCd,"¡Óá¾§à¾ªÃ")       <> 0 THEN IntS7072.RegisteredProvCd = "¡¾".
    ELSE IF index(EntS7072.RegisteredProvCd,"¢Í¹á¡è¹")         <> 0 THEN IntS7072.RegisteredProvCd = "¢¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"¨Ñ¹·ºØÃÕ")        <> 0 THEN IntS7072.RegisteredProvCd = "¨º".
    ELSE IF index(EntS7072.RegisteredProvCd,"©ÐàªÔ§à·ÃÒ")      <> 0 THEN IntS7072.RegisteredProvCd = "©ª".
    ELSE IF index(EntS7072.RegisteredProvCd,"ªÅºØÃÕ")          <> 0 THEN IntS7072.RegisteredProvCd = "ªº".
    ELSE IF index(EntS7072.RegisteredProvCd,"ªÑÂ¹Ò·")          <> 0 THEN IntS7072.RegisteredProvCd = "ª¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"ªÑÂÀÙÁÔ")         <> 0 THEN IntS7072.RegisteredProvCd = "ªÂ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ªØÁ¾Ã")           <> 0 THEN IntS7072.RegisteredProvCd = "ª¾".
    ELSE IF index(EntS7072.RegisteredProvCd,"àªÕÂ§ÃÒÂ")        <> 0 THEN IntS7072.RegisteredProvCd = "ªÃ".
    ELSE IF index(EntS7072.RegisteredProvCd,"àªÕÂ§ãËÁè")       <> 0 THEN IntS7072.RegisteredProvCd = "ªÁ".
    ELSE IF index(EntS7072.RegisteredProvCd,"µÃÑ§")            <> 0 THEN IntS7072.RegisteredProvCd = "µ§".
    ELSE IF index(EntS7072.RegisteredProvCd,"µÃÒ´")            <> 0 THEN IntS7072.RegisteredProvCd = "µÃ".
    ELSE IF index(EntS7072.RegisteredProvCd,"µÒ¡")             <> 0 THEN IntS7072.RegisteredProvCd = "µ¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã¹ÒÂ¡")         <> 0 THEN IntS7072.RegisteredProvCd = "¹Â".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã»°Á")          <> 0 THEN IntS7072.RegisteredProvCd = "¹°".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã¾¹Á")          <> 0 THEN IntS7072.RegisteredProvCd = "¹¾".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÃÒªÊÕÁÒ")      <> 0 THEN IntS7072.RegisteredProvCd = "¹Á".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÈÃÕ¸ÃÃÁÃÒª")   <> 0 THEN IntS7072.RegisteredProvCd = "¹È".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÊÇÃÃ¤ì")       <> 0 THEN IntS7072.RegisteredProvCd = "¹Ç".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹¹·ºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "¹º".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹ÃÒ¸ÔÇÒÊ")        <> 0 THEN IntS7072.RegisteredProvCd = "¹¸".
    ELSE IF index(EntS7072.RegisteredProvCd,"¹èÒ¹")            <> 0 THEN IntS7072.RegisteredProvCd = "¹¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"ºÕ§¡ÒÌ")          <> 0 THEN IntS7072.RegisteredProvCd = "º¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"ºØÃÕÃÑÁÂì")       <> 0 THEN IntS7072.RegisteredProvCd = "ºÃ".
    ELSE IF index(EntS7072.RegisteredProvCd,"»·ØÁ¸Ò¹Õ")        <> 0 THEN IntS7072.RegisteredProvCd = "»·".
    ELSE IF index(EntS7072.RegisteredProvCd,"»ÃÐ¨Çº¤ÕÃÕ¢Ñ¹¸ì") <> 0 THEN IntS7072.RegisteredProvCd = "»¢".
    ELSE IF index(EntS7072.RegisteredProvCd,"»ÃÒ¨Õ¹ºØÃÕ")      <> 0 THEN IntS7072.RegisteredProvCd = "»¨".
    ELSE IF index(EntS7072.RegisteredProvCd,"»ÑµµÒ¹Õ")         <> 0 THEN IntS7072.RegisteredProvCd = "»¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾ÃÐ¹¤ÃÈÃÕÍÂØ¸ÂÒ") <> 0 THEN IntS7072.RegisteredProvCd = "ÍÂ".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾ÐàÂÒ")           <> 0 THEN IntS7072.RegisteredProvCd = "¾Â".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾Ñ§§Ò")           <> 0 THEN IntS7072.RegisteredProvCd = "¾§".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾Ñ·ÅØ§")          <> 0 THEN IntS7072.RegisteredProvCd = "¾·".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾Ô¨ÔµÃ")          <> 0 THEN IntS7072.RegisteredProvCd = "¾¨".
    ELSE IF index(EntS7072.RegisteredProvCd,"¾ÔÉ³ØâÅ¡")        <> 0 THEN IntS7072.RegisteredProvCd = "¾Å".
    ELSE IF index(EntS7072.RegisteredProvCd,"à¾ªÃºØÃÕ")        <> 0 THEN IntS7072.RegisteredProvCd = "¾º".
    ELSE IF index(EntS7072.RegisteredProvCd,"à¾ªÃºÙÃ³ì")       <> 0 THEN IntS7072.RegisteredProvCd = "¾ª".
    ELSE IF index(EntS7072.RegisteredProvCd,"á¾Ãè")            <> 0 THEN IntS7072.RegisteredProvCd = "¾Ã".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÀÙà¡çµ")          <> 0 THEN IntS7072.RegisteredProvCd = "À¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÁËÒÊÒÃ¤ÒÁ")       <> 0 THEN IntS7072.RegisteredProvCd = "Á¤".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÁØ¡´ÒËÒÃ")        <> 0 THEN IntS7072.RegisteredProvCd = "ÁË".
    ELSE IF index(EntS7072.RegisteredProvCd,"áÁèÎèÍ§ÊÍ¹")      <> 0 THEN IntS7072.RegisteredProvCd = "ÁÊ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÂâÊ¸Ã")           <> 0 THEN IntS7072.RegisteredProvCd = "ÂÊ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÂÐÅÒ")            <> 0 THEN IntS7072.RegisteredProvCd = "ÂÅ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÃéÍÂàÍç´")        <> 0 THEN IntS7072.RegisteredProvCd = "ÃÍ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÃÐ¹Í§")           <> 0 THEN IntS7072.RegisteredProvCd = "Ã¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÃÐÂÍ§")           <> 0 THEN IntS7072.RegisteredProvCd = "ÃÂ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÃÒªºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ãº".
    ELSE IF index(EntS7072.RegisteredProvCd,"Å¾ºØÃÕ")          <> 0 THEN IntS7072.RegisteredProvCd = "Åº".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÅÓ»Ò§")           <> 0 THEN IntS7072.RegisteredProvCd = "Å»".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÅÓ¾Ù¹")           <> 0 THEN IntS7072.RegisteredProvCd = "Å¾".
    ELSE IF index(EntS7072.RegisteredProvCd,"àÅÂ")             <> 0 THEN IntS7072.RegisteredProvCd = "ÅÂ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÈÃÕÊÐà¡É")        <> 0 THEN IntS7072.RegisteredProvCd = "È¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"Ê¡Å¹¤Ã")          <> 0 THEN IntS7072.RegisteredProvCd = "Ê¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"Ê§¢ÅÒ")           <> 0 THEN IntS7072.RegisteredProvCd = "Ê¢".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊµÙÅ")            <> 0 THEN IntS7072.RegisteredProvCd = "Êµ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·Ã»ÃÒ¡ÒÃ")     <> 0 THEN IntS7072.RegisteredProvCd = "Ê»".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·ÃÊ§¤ÃÒÁ")     <> 0 THEN IntS7072.RegisteredProvCd = "ÊÊ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·ÃÊÒ¤Ã")       <> 0 THEN IntS7072.RegisteredProvCd = "Ê¤".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÃÐá¡éÇ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ê¡".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÃÐºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "Êº".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊÔ§ËìºØÃÕ")       <> 0 THEN IntS7072.RegisteredProvCd = "ÊË".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊØâ¢·ÑÂ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ê·".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊØ¾ÃÃ³ºØÃÕ")      <> 0 THEN IntS7072.RegisteredProvCd = "Ê¾".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊØÃÒÉ®Ãì¸Ò¹Õ")    <> 0 THEN IntS7072.RegisteredProvCd = "Ê®".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÊØÃÔ¹·Ãì")        <> 0 THEN IntS7072.RegisteredProvCd = "ÊÃ".
    ELSE IF index(EntS7072.RegisteredProvCd,"Ë¹Í§¤ÒÂ")         <> 0 THEN IntS7072.RegisteredProvCd = "¹¤".
    ELSE IF index(EntS7072.RegisteredProvCd,"Ë¹Í§ºÑÇÅÓÀÙ")     <> 0 THEN IntS7072.RegisteredProvCd = "¹À".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍèÒ§·Í§")         <> 0 THEN IntS7072.RegisteredProvCd = "Í·".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍÓ¹Ò¨à¨ÃÔ­")      <> 0 THEN IntS7072.RegisteredProvCd = "Í¨".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍØ´Ã¸Ò¹Õ")        <> 0 THEN IntS7072.RegisteredProvCd = "Í´".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍØµÃ´Ôµ¶ì")       <> 0 THEN IntS7072.RegisteredProvCd = "Íµ".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍØ·ÑÂ¸Ò¹Õ")       <> 0 THEN IntS7072.RegisteredProvCd = "Í¹".
    ELSE IF index(EntS7072.RegisteredProvCd,"ÍØºÅÃÒª¸Ò¹Õ")     <> 0 THEN IntS7072.RegisteredProvCd = "Íº". 
    ELSE IF index(EntS7072.RegisteredProvCd,"ºÖ§¡ÒÌ")          <> 0 THEN IntS7072.RegisteredProvCd = "º¡". 
    ELSE IF index(EntS7072.RegisteredProvCd,"àºµ§")            <> 0 THEN IntS7072.RegisteredProvCd = "ºµ". 
/*[BLANK]*/                          
    OUTPUT TO Chk_RegProvCd_Error.txt   APPEND. 
    PUT "PD_ChkDataProvincd : " TODAY FORMAT "99/99/9999" STRING(TIME,"HH:MM:SS")
        "Company:"                   EntS7072.company          FORMAT "X(10)"  
        "ContractNumber:"            EntS7072.ContractNumber   FORMAT "X(30)"  
        "EntS7072.RegisteredProvCd:" EntS7072.RegisteredProvCd FORMAT "X(35)"  
        "IntS7072.RegisteredProvCd:" IntS7072.RegisteredProvCd FORMAT "X(35)" SKIP. 
    OUTPUT CLOSE. 
END.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ClearData C-Win 
PROCEDURE PD_ClearData :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
    ASSIGN    fi_PolicyNumber = "" fi_Vehicle     = "" fi_vehreg = ""
    fi_RegisteredProvinceCode = "" fi_PlateNumber = ""
    .
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ConDBExp C-Win 
PROCEDURE PD_ConDBExp :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[COMMENT]*/                       /*      19.30.00 ¹.      07.30.00 ¹.*/
IF TIME >= 70200 OR TIME <= 27000 THEN DO:
/*[BLANK]*/                      
  IF CONNECTED ("expiry") THEN DO:
/*[BLANK]*/                      
    DISCONNECT expiry NO-ERROR.
/*[BLANK]*/                      
    OUTPUT TO DBExpiry.TXT APPEND.
    PUT "PD_SAVEPD1: 1. DISCONNECT DB Expiry: " FORMAT "x(36)"
         CONNECTED ("expiry")
        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
  END.
/*[BLANK]*/                      
END.
ELSE DO:
/*[BLANK]*/                      
  IF NOT CONNECTED ("expiry") THEN DO:
/*[BLANK]*/                      
    RUN WRS/WRSGU1DB.P
         (""  /*Userid*/
         ,""  /*Password*/
         ,"expiry"). /*Database name*/
/*[BLANK]*/                      
    OUTPUT TO DBExpiry.TXT APPEND.
    PUT "PD_SAVEPD1: 2. CONNECTED DB Expiry: " FORMAT "x(36)"
         CONNECTED ("expiry")
        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
  END.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pd_DeleteResultX C-Win 
PROCEDURE Pd_DeleteResultX :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT-OUTPUT PARAMETER nv_rec_rq AS RECID     NO-UNDO.
/*[BLANK]*/                      
/*[BLANK]*/                      
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntS7072 THEN RETURN.
ELSE DO:
/*[BLANK]*/                      
  FOR EACH IntS7072Result WHERE
           IntS7072Result.SystemRq      = IntS7072.SystemRq
       AND IntS7072Result.CompanyCode   = IntS7072.CompanyCode
       AND IntS7072Result.PolicyNumber  = IntS7072.PolicyNumber
  :
/*[BLANK]*/                      
    DELETE IntS7072Result.
  END.
/*[BLANK]*/                      
  DELETE IntS7072.
/*[BLANK]*/                      
  nv_rec_rq = 0.
END.
/*[BLANK]*/                      
RELEASE IntS7072Result.
RELEASE IntS7072.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_DispData C-Win 
PROCEDURE PD_DispData :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
    DISPLAY fi_PolicyNumber            fi_Vehicle      fi_vehreg 
            fi_RegisteredProvinceCode  fi_PlateNumber
    WITH FRAME DEFAULT-FRAME.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_DispMess C-Win 
PROCEDURE PD_DispMess :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT PARAMETER nv_notfound AS CHARACTER NO-UNDO.
ASSIGN
/*[COMMENT]*/                        /*
/*[COMMENT]*/                        fi_notfound2 = "Please press button F4 = TO Exit. " 
/*[COMMENT]*/                                     + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").
/*[COMMENT]*/                        my-datetime  = STRING(DATETIME(TODAY, MTIME)).                  
/*[COMMENT]*/                        */
    fi_notfound  = nv_notfound 
    my-datetime  = SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12) + " "
                 + SUBSTR(STRING(DATETIME(TODAY, MTIME)),1,10)
    fi_notfound2 = my-datetime + "    Please press button F4 = TO Exit & ÃÍÊÑ¡¤ÃÙè ("
                 + STRING(nv_CountLeave,">9") + ")".
/*[BLANK]*/                      
    DISPLAY  fi_notfound fi_notfound2 FORMAT "X(78)" FGCOLOR 6 WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ErrorRunPol C-Win 
PROCEDURE PD_ErrorRunPol :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /**/
DEFINE INPUT PARAMETER nv_rec_rq     AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER nv_msgerror   AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_PolicyV72  AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    DEFINE VARIABLE nv_AddData           AS LOGICAL   NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Cpolicy           AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_CBarCodeNumber    AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    ASSIGN
/*[COMMENT]*/                    nv_Cpolicy        = ""
/*[COMMENT]*/                    nv_CBarCodeNumber = ""
/*[COMMENT]*/                    nv_resulttext     = "".
/*[COMMENT]*/                    */
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF AVAILABLE IntS7072 THEN DO:
/*[BLANK]*/                      
  FIND FIRST EntS7072 WHERE
             EntS7072.SystemRq        = IntS7072.SystemRq
         AND EntS7072.CompanyCode     = IntS7072.CompanyCode
         AND EntS7072.Username        = IntS7072.Username
         AND EntS7072.Password        = IntS7072.Password
         AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[COMMENT]*/                           /*AND EntS7072.PolicyNumber = nv_PolicyV70 */
  NO-ERROR NO-WAIT.
  IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
  IntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
  RUN PD_SaveError (nv_msgerror).
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RUN PD_SaveError ("äÁè¾ºÃËÑÊ¾Ãº. " + IntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ").
/*[COMMENT]*/                      */
/*[BLANK]*/                      
END.
/*[BLANK]*/                      
RETURN.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_FNameAttach C-Win 
PROCEDURE PD_FNameAttach :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
DEFINE INPUT        PARAMETER nv_CompanyCode     AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER nv_PolicyType      AS CHARACTER NO-UNDO. /*v70,v72*/
DEFINE INPUT        PARAMETER nv_CMIPolicyTypeCd AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd      AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_PrgName         AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg          AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
DEFINE VAR nv_ONLine    AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    nv_PolicyType  = "V70".
/*[COMMENT]*/                    nv_CompanyCode = "210".
/*[COMMENT]*/                    nv_CMIPolicyTypeCd = "3".
/*[COMMENT]*/                    */
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN
  nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[BLANK]*/                      
IF IntPol7072.SERVICE_ID = "online" THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                      /*ASN = ãºÃÑºÃÍ§¡ÒÃÃÑº»ÃÐ¡Ñ¹ÀÑÂ */
  nv_ONLine = TRIM(nv_CompanyCode) + "cer".
/*[BLANK]*/                       
/*[BLANK]*/                      
  FIND FIRST FNameAttach WHERE
             FNameAttach.CompanyCode  = nv_ONLine
         AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
         AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                             /*AND FNameAttach.EffDate     <= TODAY*/
         AND FNameAttach.SelectNumber = 1
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE FNameAttach THEN
    ASSIGN 
    nv_NameCompCd = FNameAttach.CompanyCode
    nv_PrgName    = FNameAttach.PrgName
    nv_PrmPrg     = FNameAttach.PrmPrg.
/*[BLANK]*/                      
  IF nv_NameCompCd = "" THEN DO:
/*[BLANK]*/                      
    nv_ONLine = "ALLcer".
/*[BLANK]*/                      
    FIND FIRST FNameAttach WHERE
               FNameAttach.CompanyCode  = nv_ONLine
           AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
           AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                               /*AND FNameAttach.EffDate     <= TODAY   */           /*08/01/2014*/
           AND FNameAttach.SelectNumber = 1
    NO-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE FNameAttach THEN
      ASSIGN
      nv_NameCompCd = "ALLcer"
      nv_PrgName    = FNameAttach.PrgName
      nv_PrmPrg     = FNameAttach.PrmPrg.
  END.
END.
ELSE DO:
/*[BLANK]*/                      
  FIND FIRST FNameAttach WHERE
             FNameAttach.CompanyCode  = nv_CompanyCode
         AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
         AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                             /*AND FNameAttach.EffDate     <= TODAY*/
         AND FNameAttach.SelectNumber = 1
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE FNameAttach THEN
    ASSIGN 
    nv_NameCompCd = FNameAttach.CompanyCode
    nv_PrgName    = FNameAttach.PrgName
    nv_PrmPrg     = FNameAttach.PrmPrg.
/*[BLANK]*/                      
  IF nv_NameCompCd = "" THEN DO:
/*[BLANK]*/                      
    FIND FIRST FNameAttach WHERE
               FNameAttach.CompanyCode  = "ALL"
           AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
           AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                               /*AND FNameAttach.EffDate     <= TODAY  */            /*08/01/2014*/
           AND FNameAttach.SelectNumber = 1
    NO-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE FNameAttach THEN
      ASSIGN
      nv_NameCompCd = "ALL"
      nv_PrgName    = FNameAttach.PrgName
      nv_PrmPrg     = FNameAttach.PrmPrg.
  END.
/*[BLANK]*/                      
END.
/*[BLANK]*/                      
OUTPUT TO PD_FNameAttach.TXT APPEND.
PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    nv_CompanyCode     FORMAT "x(20)"
    nv_PolicyType      FORMAT "x(20)"
    nv_CMIPolicyTypeCd FORMAT "x(20)"
  nv_NameCompCd FORMAT "x(20)" /*210*/
  nv_PrgName    FORMAT "x(20)" /*Wctxr701A4*/
  nv_PrmPrg     FORMAT "x(20)" /*V70A4*/
  nv_ONLine     FORMAT "x(20)" /*V70A4*/
SKIP.
OUTPUT CLOSE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      DISPLAY nv_NameCompCd FORMAT "x(25)" /*210*/
/*[COMMENT]*/                              nv_PrgName    FORMAT "x(25)" /*Wctxr701A4*/
/*[COMMENT]*/                              nv_PrmPrg     FORMAT "x(25)" /*V70A4*/
/*[COMMENT]*/                      WITH 1 COLUMN.
/*[COMMENT]*/                    */
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_Futil C-Win 
PROCEDURE PD_Futil :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[COMMENT]*/                    /*
/*[BLANK]*/                      
/*[COMMENT]*/                    DEFINE INPUT PARAMETER prm_PolicyTypeCd AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT PARAMETER prm_SumInsureAmt AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT PARAMETER prm_CompanyCode  AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    DEFINE INPUT-OUTPUT PARAMETER nv_okprn      AS LOGICAL   NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    DEFINE VARIABLE nv_UtilGrp    AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Sumins     AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[COMMENT]*/                    nv_okprn = NO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*§Ò¹»2.1 2.2 ·Ø¹ 1áÊ¹/ 2áÊ¹ ãËé¾ÔÁ¾ì ¡ÃÁ¸ÃÃÁìä´é*/
/*[BLANK]*/                      
/*[COMMENT]*/                    IF    prm_PolicyTypeCd = "2.1" OR prm_PolicyTypeCd = "2.6"
/*[COMMENT]*/                       OR prm_PolicyTypeCd = "2.2" OR prm_PolicyTypeCd = "2.7"
/*[COMMENT]*/                       OR prm_PolicyTypeCd = "2.3" OR prm_PolicyTypeCd = "2.8"
/*[COMMENT]*/                       OR prm_PolicyTypeCd = "2.4" OR prm_PolicyTypeCd = "2.9"
/*[COMMENT]*/                       OR prm_PolicyTypeCd = "2.5"
/*[COMMENT]*/                    THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                           IF prm_PolicyTypeCd = "2.1" THEN nv_UtilGrp = "Prn21".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.2" THEN nv_UtilGrp = "Prn22".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.3" THEN nv_UtilGrp = "Prn23".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.4" THEN nv_UtilGrp = "Prn24".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.5" THEN nv_UtilGrp = "Prn25".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.6" THEN nv_UtilGrp = "Prn26".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.7" THEN nv_UtilGrp = "Prn27".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.8" THEN nv_UtilGrp = "Prn28".
/*[COMMENT]*/                      ELSE IF prm_PolicyTypeCd = "2.9" THEN nv_UtilGrp = "Prn29".
/*[BLANK]*/                      
/*[COMMENT]*/                      nv_Sumins = prm_SumInsureAmt.
/*[BLANK]*/                      
/*[COMMENT]*/                      IF INDEX(nv_Sumins,".") <> 0 THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        nv_Sumins = SUBSTR(nv_Sumins,1,INDEX(nv_Sumins,".") - 1).
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      nv_Sumins = REPLACE(nv_Sumins,",","") .
/*[BLANK]*/                      
/*[COMMENT]*/                      FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                                 FUtilSetUp.UtilGrp     = nv_UtilGrp
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp1 = prm_PolicyTypeCd
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp2 = prm_CompanyCode
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp3 = "V70"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp4 = "Policy"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp5 = nv_Sumins
/*[COMMENT]*/                             AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                      NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                      IF AVAILABLE FUtilSetUp THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        IF FUtilSetUp.UtilGrpCd1 = "YES" THEN nv_okprn = YES.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    */
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_PUTError C-Win 
PROCEDURE PD_PUTError :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
DEFINE INPUT PARAMETER nv_noerror AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
IF nv_noerror = "2" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO WSPGP100-ERROR.TXT APPEND.
  PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
      TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "3" THEN DO:
  OUTPUT TO WRSGU100-ERROR.TXT APPEND.
  PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
    TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "4" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO WSPGP100-ERROR.TXT APPEND.
  PUT "4. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
    TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "5" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO WRSGU100-ERROR.TXT APPEND.
  PUT "5. WRSGU10R.P " IntPol7072.PolicyNumber FORMAT "X(18)"
   TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
   " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "6" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO WRSGU100-ERROR.TXT APPEND.
  PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
    TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "Rer" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO WRSGU100-ERROR-RENEW.TXT APPEND.
  PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
    TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    " " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "PD01" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO PD_SAVEPD1_01.TXT APPEND.
  PUT "PD_SAVEPD1_01" FORMAT "x(13)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    nv_msgerror7072 FORMAT "x(130)"
  SKIP.
  OUTPUT CLOSE.
  RETURN. /*Add kridtiya i. ÂØº Loop */
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "PD1" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO PD_SAVEPD1.TXT APPEND.
  PUT "PD_SAVEPD1: 1. START " FORMAT "x(25)"
      " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      " " IntPol7072.PolicyNumber
      " " IntPol7072.PreviousPolicyNumber
  SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "PD2" THEN DO:
/*[BLANK]*/                      
  OUTPUT TO PD_SAVEPD1.TXT APPEND.
  PUT "PD_SAVEPD1: 2. END " FORMAT "x(25)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
  SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "EX1" THEN DO:
  OUTPUT TO EXPIRY-DATA.TXT APPEND.
  PUT "1. EXPIRY " nv_PolicyV70 FORMAT "X(18)"
      TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "EX2" THEN DO:
  OUTPUT TO EXPIRY-DATA.TXT APPEND.
  PUT "2. EXPIRY: " nv_PolicyV70 FORMAT "X(18)"
      TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IF nv_noerror = "EXNOTPRM1" THEN DO:
  OUTPUT TO EXPIRY-ERROR.TXT APPEND.
  PUT "1.EXPIRY ·Ø¹ /àºÕéÂ /covcod äÁèµÃ§: " nv_PolicyV70 FORMAT "X(16)"
      "PreviousPolicyNumber: " IntPol7072.PreviousPolicyNumber FORMAT "X(16)"
      TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
END.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pd_saveCer C-Win 
PROCEDURE Pd_saveCer :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER nv_COPYTOFILE  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER nv_RECIDcer    AS RECID NO-UNDO.
DEFINE  VARIABLE    CompanyCode    AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    Username       AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    BrancdCd       AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    ContractNumber AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    PolicyNumber   AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    nv_FileName    AS CHARACTER NO-UNDO.
DEFINE  VARIABLE    PolicyBinRs    AS MEMPTR    NO-UNDO. 
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    OUTPUT TO PD_SaveCerf_Log.TXT APPEND.
/*[COMMENT]*/                    PUT
/*[COMMENT]*/                        TODAY    FORMAT "99/99/9999"  " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    "IN nv_COPYTOFILE:"  nv_COPYTOFILE  FORMAT "X(30)"  
/*[COMMENT]*/                    "nv_RECIDcer  :"  nv_RECIDcer   FORMAT ">>>>>>>>>>9"   SKIP .
/*[COMMENT]*/                    OUTPUT CLOSE.
/*[BLANK]*/                      
/*[COMMENT]*/                    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDcer 
/*[COMMENT]*/                         NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF  AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                      /*  
/*[COMMENT]*/                    CompanyCode    = "833".
/*[COMMENT]*/                    Username       = "pdmgr0".
/*[COMMENT]*/                    BrancdCd       = "0".
/*[COMMENT]*/                    ContractNumber = "123456789".
/*[COMMENT]*/                    PolicyNumber   = "DT7063C00001".
/*[COMMENT]*/                     nv_FileName    = "D:\DT7063C00001.pdf". */ 
/*[COMMENT]*/                        IF IntPol7072.CMIPolicyTypeCd =  "" THEN DO:
/*[COMMENT]*/                            RUN wrs\wrsaddcerpdf.p (INPUT  IntPol7072.CompanyCode
/*[COMMENT]*/                            /*RUN wrs\wrsaddcerdpa.p (INPUT  IntPol7072.CompanyCode*/
/*[COMMENT]*/                                                    ,input  "pdmgr0"    
/*[COMMENT]*/                                                    ,input  IntPol7072.BranchCd  
/*[COMMENT]*/                                                    ,input  IntPol7072.ContractNumber
/*[COMMENT]*/                                                    ,input  IntPol7072.PolicyNumber
/*[COMMENT]*/                                                    ,input  nv_COPYTOFILE
/*[COMMENT]*/                                                    ,OUTPUT PolicyBinRs).  
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                            RUN wrs\wrsaddcerpdf.p (INPUT  IntPol7072.CompanyCode
/*[COMMENT]*/                            /*RUN wrs\wrsaddcerdpa.p (INPUT  IntPol7072.CompanyCode*/
/*[COMMENT]*/                                                    ,input  "pdmgr0"    
/*[COMMENT]*/                                                    ,input  IntPol7072.BranchCd  
/*[COMMENT]*/                                                    ,input  IntPol7072.ContractNumber
/*[COMMENT]*/                                                    ,input  IntPol7072.CMIPolicyNumber
/*[COMMENT]*/                                                    ,input  nv_COPYTOFILE
/*[COMMENT]*/                                                    ,OUTPUT PolicyBinRs). 
/*[BLANK]*/                      
/*[COMMENT]*/                        END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    OUTPUT TO PD_SaveCerf_Log.TXT APPEND.
/*[COMMENT]*/                    PUT
/*[COMMENT]*/                        TODAY    FORMAT "99/99/9999"  " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    "OUT nv_COPYTOFILE:"  nv_COPYTOFILE  FORMAT "X(30)"  
/*[COMMENT]*/                    "nv_RECIDcer  :"  nv_RECIDcer    FORMAT ">>>>>>>>>>9"   SKIP .
/*[COMMENT]*/                    OUTPUT CLOSE.*/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SaveError C-Win 
PROCEDURE PD_SaveError :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_ErrorMessage  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
    FIND FIRST EntS7072Result WHERE
               EntS7072Result.SystemRq        = IntS7072.SystemRq
           AND EntS7072Result.CompanyCode     = IntS7072.CompanyCode
           AND EntS7072Result.ContractNumber  = IntS7072.ContractNumber
           AND EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq
    NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                         
      CREATE EntS7072Result.
/*[BLANK]*/                         
      ASSIGN
      EntS7072Result.SystemRq             = IntS7072.SystemRq
      EntS7072Result.InsurerId            = IntS7072.InsurerId
      EntS7072Result.RqUID                = IntS7072.RqUID
      EntS7072Result.keyRequestIndRq      = IntS7072.keyRequestIndRq
/*[COMMENT]*/                          /**/
      EntS7072Result.CompanyCode          = IntS7072.CompanyCode
      EntS7072Result.ContractNumber       = IntS7072.ContractNumber
/*[BLANK]*/                      
      EntS7072Result.ReferenceNumber      = IntS7072.ReferenceNumber
      EntS7072Result.EndorseRefNumber     = IntS7072.EndorseRefNumber
/*[COMMENT]*/                          /**/
      EntS7072Result.InsurerCode          = IntS7072.InsurerCode
      EntS7072Result.MethodCode           = IntS7072.MethodCode
      EntS7072Result.Policy               = IntS7072.Policy
      EntS7072Result.Rencnt               = IntS7072.Rencnt
      EntS7072Result.Endcnt               = IntS7072.Endcnt
      EntS7072Result.Riskno               = IntS7072.Riskno
      EntS7072Result.Itemno               = IntS7072.Itemno
/*[COMMENT]*/                          /**/
      EntS7072Result.ProcessByUser        = IntS7072.ProcessByUser
      EntS7072Result.ProcessDate          = IntS7072.ProcessDate
      EntS7072Result.ProcessTime          = IntS7072.ProcessTime
/*[COMMENT]*/                          /**/
      EntS7072Result.TrnFromIntDate       = IntS7072.TrnFromIntDate
      EntS7072Result.TrnFromIntTime       = IntS7072.TrnFromIntTime
      EntS7072Result.ReceiveNumber        = IntS7072.ReceiveNumber
      EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[COMMENT]*/                          /**/
      EntS7072Result.RecordGUIDRs         = ""
/*[COMMENT]*/                          /**/
      EntS7072Result.TransactionResponseDt   =   STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
                                               + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
                                               + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
      EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[COMMENT]*/                          /**/
      EntS7072Result.PolicyNumber         = ""
      EntS7072Result.DocumentUID          = ""
/*[COMMENT]*/                          /**/
      EntS7072Result.CMIPolicyNumber      = ""
      EntS7072Result.CMIDocumentUID       = ""
      EntS7072Result.MsgStatusCd          = "FAIL"
/*[BLANK]*/                      
      EntS7072Result.BranchCd             = IntS7072.BranchCd
      EntS7072Result.vehreg               = IntS7072.vehreg
      EntS7072Result.Adjustno             = 0
/*[COMMENT]*/                          /**/
      EntS7072Result.ResultStatus         = "FAIL"                 /*IntS7072.ResultStatus*/
      EntS7072Result.ErrorCode            = ""
      EntS7072Result.ErrorMessage         = nv_ErrorMessage
/*[COMMENT]*/                          /**/
      EntS7072Result.LinkStatus           = "E"      .
    END.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1 C-Win 
PROCEDURE PD_SAVEPD1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER  nv_recinout7072   AS RECID NO-UNDO.
DEFINE VARIABLE nv_trty11  AS CHARACTER NO-UNDO. /*M,T*/
DEFINE VARIABLE nv_docno1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_STdocno AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_STKNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_okprn AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_OLDPolicy AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_NewPolicy AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
nv_msgerror7072  = "".   /* Add kridtiya i. */
RUN PD_ConDBExp.         /* DB EXPIRY */
/*[COMMENT]*/                    /* ------------------------------------------------------ */
nv_resulttext = "".
nv_octets = "".
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[COMMENT]*/                    /*¡ÃÁ¸ÃÃÁì»... + ¾Ãº. */
IF (IntS7072.PolicyTypeCd    <> "" ) AND (IntS7072.RateGroup    <> "" ) AND  /*Add kridtiya i. */
   (IntS7072.CMIPolicyTypeCd <> "" ) AND (IntS7072.CMIVehTypeCd <> "" )
THEN DO:
/*[COMMENT]*/                      /*RUN PD_SAVEPD17072.*/
  FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70  /*70,72*/
  NO-ERROR NO-WAIT.
  IF NOT AVAILABLE IntPol7072 THEN DO:
    RUN WRS/WRSDigit.p (output nv_octets).
    CREATE IntPol7072.
  END.
  ELSE nv_octets = IntPol7072.RqUID.
END.
ELSE IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN DO:
  nv_octets = "".
  FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70 /*70,72*/
  NO-ERROR NO-WAIT.
  IF NOT AVAILABLE IntPol7072 THEN DO:
    RUN WRS/WRSDigit.p (output nv_octets).
    CREATE IntPol7072.
  END.
  ELSE nv_octets = IntPol7072.RqUID.
END.
ELSE DO:   /* ¡ÃÁ¸ÃÃÁì ¾Ãº.*/
  IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
    FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72
    NO-ERROR NO-WAIT.
    IF NOT AVAILABLE IntPol7072 THEN DO:
      nv_octets = "".
      RUN WRS/WRSDigit.p (output nv_octets).
      CREATE IntPol7072.
    END.
    ELSE nv_octets = IntPol7072.RqUID.
  END.
  ELSE nv_msgerror7072 = "¾º¢éÍÁÙÅ ¾Ãº.äÁè¶Ù¡µéÍ§ CMIPolicyTypeCd /CMIVehTypeCd à»ç¹¤èÒÇèÒ§".
END.
nv_RecIntPol7072 = RECID(IntPol7072).
nv_recinout7072  = RECID(IntPol7072).
RUN PD_SAVEPD2.    /*Save data to IntPol7072*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[COMMENT]*/                    /* ËÒàÅ¢ Docno1 */
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntS7072 THEN RETURN.
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    /*20/3/2015*/
nv_okprn = NO.
RUN WRS\WRSFutil.p 
    (IntPol7072.PolicyTypeCd
    ,IntPol7072.SumInsureAmt
    ,IntPol7072.CompanyCode
    ,INPUT-OUTPUT nv_okprn).
/*[COMMENT]*/                    /* ---------------------------------------------------- */
RUN PD_SAVEPD1_01 (nv_okprn 
                  ,nv_trty11            
                  ,nv_docno1            
                  ,nv_STdocno
                  ,nv_STKNo).  /*Add kridtiya i. ÂØº Loop */
IF IntPol7072.CMIDocumentUID <> "" THEN IntS7072.CMIDocumentUID = IntPol7072.CMIDocumentUID .
IF nv_msgerror7072 <> "" THEN DO:
  RUN PD_PUTError ("PD01").
  RETURN. /*Add kridtiya i. ÂØº Loop */
END.
IF IntPol7072.GenSicBranST   = "ERROR" THEN RETURN. /*Add kridtiya i. ÂØº Loop */
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[BLANK]*/                      
RUN PD_PUTError ("PD1").
/*[BLANK]*/                      
nv_Renew = NO.  /*A57-0300: §Ò¹µèÍÍÒÂØ*/
/*[BLANK]*/                      
IF  (SUBSTR(IntPol7072.PolicyNumber,1,1)    =  "R") OR 
    (IntPol7072.PolicyStatus  = "R" AND IntPol7072.PreviousPolicyNumber <> "")
    AND    IntPol7072.PreviousPolicyNumber <> ""
THEN nv_Renew = YES.
IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN nv_Renew = NO. /*¾Ãº.*/
/*[BLANK]*/                      
ASSIGN
nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
/*[COMMENT]*/                    /*nv_msgerror7072  = "".*/ /* Add kridtiya i. Â¡ä» ´éÒ¹º¹ÊØ´ */
/*[COMMENT]*/                    /* Generate policy, uwm100, uwm120, uwm301, uwm130, uwd132 */
/*[BLANK]*/                      
IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN DO: 
   RUN PD_SAVEPD1_02.   /*Add kridtiya i. 70 + ¾Ãº. */
   IF nv_msgerror7072 <> "" THEN RETURN.
END.
ELSE IF IntPol7072.PolicyTypeCd <> "1" 
    AND IntPol7072.PolicyTypeCd <> "2"
    AND IntPol7072.PolicyTypeCd <> "3"
    AND nv_Renew = NO  /*Add by Kridtiya i. 20/06/2017 */
THEN DO:
/*[BLANK]*/                      
  IF    IntPol7072.PolicyTypeCd = ""
     OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
     OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[COMMENT]*/                         /**/
     OR IntPol7072.CMIPolicyTypeCd = "110"
     OR IntPol7072.CMIPolicyTypeCd = "120A"
     OR IntPol7072.CMIPolicyTypeCd = "140A"
  THEN DO:
    IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
      nv_msgerror7072 = "".
      RUN UZ/UZO7201WS.P
           (nv_RecIntPol7072
           ,INPUT-OUTPUT nv_msgerror7072).
      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
      NO-ERROR.
      IF nv_msgerror7072 <> "" THEN DO:
        RUN PD_PUTError ("2").
      END.
    END. /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
    IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
       OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
    THEN DO:
/*[BLANK]*/                      
      RUN WRS/WRSGU100.P
           (nv_RecIntPol7072
           ,INPUT-OUTPUT nv_msgerror7072).
      IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
        RUN PD_PUTError ("3").
/*[BLANK]*/                      
      END.
    END.
  END.
  ELSE DO:
    IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[BLANK]*/                      
      RUN UZ/UZO7201WS.P
           (nv_RecIntPol7072
           ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
      NO-ERROR.
      IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
        RUN PD_PUTError ("4").
/*[BLANK]*/                      
      END.
    END.
  END.
/*[BLANK]*/                      
END.
ELSE DO:
/*[COMMENT]*/                      /* A57-0300: §Ò¹µèÍÍÒÂØ */
/*[COMMENT]*/                      /* ISUZU */
  IF     IntPol7072.PolicyStatus  = "R"   AND IntPol7072.PreviousPolicyNumber <> ""
     AND IntPol7072.PolicyTypeCd <> ""  
/*[COMMENT]*/                       /*  AND IntPol7072.CompanyCode   = "476" AND SUBSTR(IntPol7072.PolicyNumber,1,2) = "74"*/ /*comment by Kridtiya i. 17/06/2017*/
  THEN nv_Renew = YES.
/*[BLANK]*/                      
  ASSIGN 
  nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
  nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
  IF nv_Renew = YES THEN DO:
/*[BLANK]*/                      
    IF NOT CONNECTED ("expiry") THEN DO:
      RUN WRS/WRSGU1DB.P
           (""  /*Userid*/
          ,""  /*Password*/
          ,"expiry"). /*Database name*/
    END.
/*[BLANK]*/                      
    IF CONNECTED ("expiry") THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          /* Check ·Õè Pc_CheckDataExt ¡èÍ¹áÅéÇ ----------------
/*[COMMENT]*/                             /*ISUZU*/
/*[COMMENT]*/                          IF IntPol7072.CompanyCode = "476" THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                            RUN WRS/WRSGUCKE.P /*Check ·Ø¹ /àºÕéÂ /covcod ·Õè expiry µÃ§¡Ñ¹ËÃ×ÍäÁè*/
/*[COMMENT]*/                              (nv_RecIntPol7072
/*[COMMENT]*/                               /**/
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_expiryrencnt /*expiry.uwm100.rencnt*/
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_expirysigr_p
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_expiryprem_t
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_expirysclass
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_expirycovcod
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_PrnRenew
/*[COMMENT]*/                               /**/
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[COMMENT]*/                            RUN PD_PUTError ("EX1").
/*[BLANK]*/                      
/*[COMMENT]*/                            nv_msgerror7072 = "".
/*[BLANK]*/                      
/*[COMMENT]*/                            IF nv_PrnRenew = "YES" THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                              RUN WRS/WRSGU10R.P
/*[COMMENT]*/                                  (nv_RecIntPol7072
/*[COMMENT]*/                                  ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                            END.
/*[COMMENT]*/                            ELSE nv_msgerror7072 = nv_PolicyV70 + " PrnRenew/¢éÍÁÙÅµÃ§¡Ñ¹: ".
/*[BLANK]*/                      
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          ELSE DO:
/*[BLANK]*/                      
/*[COMMENT]*/                            RUN WRS/WRSGU10R.P
/*[COMMENT]*/                                (nv_RecIntPol7072
/*[COMMENT]*/                                ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          ------------------------------------------------ */
      RUN WRS/WRSGU10R.P
             (nv_RecIntPol7072
            ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
      NO-ERROR NO-WAIT.
/*[BLANK]*/                      
      IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
        RUN PD_PUTError ("Rer").
/*[BLANK]*/                      
      END.
/*[COMMENT]*/                          /********** comment by Kridtiya i. 26/08/2017   /*ISUZU*/
/*[COMMENT]*/                          IF IntPol7072.CompanyCode = "476" OR IntPol7072.CompanyCode = "833"  OR IntPol7072.CompanyCode = "834" THEN DO: /*Add by Kridtiya i. 17/06/2017*/
/*[COMMENT]*/                              */
/*[BLANK]*/                            
        IF nv_msgerror7072 = "" THEN DO:
          ASSIGN
          nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
          nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
          RUN WRS/WRSGUCKR.P /*Check ·Ø¹ /àºÕéÂ /covcod ·Õè sic_bran µÃ§ËÃ×ÍäÁè*/
            (nv_RecIntPol7072
/*[COMMENT]*/                                 /**/
            ,INPUT-OUTPUT nv_expiryrencnt
            ,INPUT-OUTPUT nv_expirysigr_p
            ,INPUT-OUTPUT nv_expiryprem_t
            ,INPUT-OUTPUT nv_expirysclass
            ,INPUT-OUTPUT nv_expirycovcod
            ,INPUT-OUTPUT nv_PrnRenew
/*[COMMENT]*/                                 /**/
            ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
          RUN PD_PUTError ("EX2").
/*[BLANK]*/                      
/*[BLANK]*/                      
          nv_msgerror7072 = "".
/*[BLANK]*/                      
          IF IntPol7072.PolicyTypeCd = "1" THEN nv_PrnRenew = "YES".
/*[BLANK]*/                      
/*[COMMENT]*/                              /* Change R -> D */
/*[BLANK]*/                      
          IF nv_PrnRenew = "YES" THEN DO: /*·Ø¹ /àºÕéÂ /covcod µÃ§*/
/*[COMMENT]*/                                  /* àÅ¢ R */
            IF (SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R") AND (IntPol7072.CompanyCode = "476") THEN DO:
/*[BLANK]*/                      
              nv_NewPolicy = "".
              nv_OLDPolicy = IntPol7072.PolicyNumber.
/*[BLANK]*/                      
              RUN WSP/WSPRunP2WS.P  (INPUT  IntPol7072.CompanyCode
                                    ,INPUT  IntPol7072.BranchCd
                                    ,INPUT  "V70"
                                    ,OUTPUT nv_NewPolicy
                                    ,OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
              FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
              NO-ERROR NO-WAIT.
/*[BLANK]*/                      
              OUTPUT TO WRSBQ7072-Renew.TXT APPEND.
              PUT "Change R -> D: " IntPol7072.CompanyCode IntPol7072.BranchCd
                  nv_OLDPolicy FORMAT "x(16)" nv_NewPolicy FORMAT "x(16)"
                  SKIP.
              OUTPUT CLOSE.
/*[BLANK]*/                      
              IF nv_msgerror7072 = "" AND nv_NewPolicy <> "" THEN DO:
/*[BLANK]*/                      
                RUN WRS/WRSCngPoL.P
                  (INPUT nv_OLDPolicy
                  ,INPUT nv_expiryrencnt
                  ,INPUT 0 /*endcnt*/
                  ,INPUT nv_NewPolicy
/*[COMMENT]*/                                       /**/
                  ,OUTPUT nv_msgerror7072).
              END.
/*[BLANK]*/                      
              IF nv_msgerror7072 = "" AND nv_NewPolicy <> ""  THEN DO:
/*[BLANK]*/                      
                FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
                NO-ERROR NO-WAIT.
                IF AVAILABLE IntPol7072 THEN
                  ASSIGN
                  nv_PolicyV70 = nv_NewPolicy
                  IntPol7072.PolicyNumber = nv_NewPolicy
                  IntPol7072.ConfirmBy = "AUTO".
/*[BLANK]*/                      
/*[COMMENT]*/                                    /*??? àÅ¢ docno ËÒÍÂèÒ§äÃ*/
/*[BLANK]*/                      
              END.
            END. /*IF SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R"*/ 
/*[BLANK]*/                      
          END. /* nv_PrnRenew = "YES" */
          ELSE DO:
/*[BLANK]*/                      
            nv_msgerror7072 = "Not renew: " + nv_PrnRenew.
          END.
        END.
/*[COMMENT]*/                          /*END. /*IF IntPol7072.CompanyCode = "476" THEN DO:*//* comment by Kridtiya i. 15/07/2017*/*/ /*comment by Kridtiya i. 26/08/2017 */
/*[BLANK]*/                      
    END.
    ELSE  nv_msgerror7072 = "Not Connect Expiry: ".
/*[COMMENT]*/                        /**/
/*[BLANK]*/                      
    IF nv_PrnRenew = "" THEN DO: /*·Ø¹ /àºÕéÂ /covcod äÁèµÃ§*/
/*[BLANK]*/                      
      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
      NO-ERROR.
      IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
        RUN PD_PUTError ("5").
/*[BLANK]*/                      
        nv_msgerror7072 = "".
/*[BLANK]*/                      
        RUN PD_PUTError ("EXNOTPRM1"). /*·Ø¹ /àºÕéÂ /covcod äÁèµÃ§*/
/*[BLANK]*/                      
        RUN WRS/WRSGU100.P
           (nv_RecIntPol7072
           ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
      END.
    END.
/*[BLANK]*/                      
    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
    NO-ERROR.
  END.
  ELSE DO:
/*[COMMENT]*/                        /*äÁèãªè§Ò¹ Renew*/
/*[BLANK]*/                      
    IF IntPol7072.PolicyTypeCd <> "1"  THEN DO:
/*[BLANK]*/                      
      RUN WRS/WRSGU100.P
         (nv_RecIntPol7072
         ,INPUT-OUTPUT nv_msgerror7072).
    END.
    ELSE DO:
      IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.PolicyTypeCd = "1" THEN DO:
        nv_acno1 = "".
/*[COMMENT]*/                            /* ËÒ ÃËÑÊµÑÇá·¹  */
        RUN WSP/WSPMCpny.P 
            (IntPol7072.CompanyCode
            ,IntPol7072.BranchCd
            ,"V" + SUBSTR(IntPol7072.PolicyNumber,3,2) /*nv_Poltyp*/
            ,OUTPUT crCompanyNo
            ,OUTPUT crBranchNo
            ,OUTPUT nv_acno1
            ,OUTPUT nv_agent
            ,OUTPUT nv_msgerror7072
            ).
/*[COMMENT]*/                            /**/
        IF nv_acno1 <> "" THEN DO:
          FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
          NO-ERROR.
          IF AVAILABLE IntPol7072 THEN 
            IntPol7072.AgentBrokerLicenseNumber = nv_acno1.
        END.
      END.
      ELSE DO:
/*[BLANK]*/                      
        RUN WRS/WRSGU100.P
            (nv_RecIntPol7072
            ,INPUT-OUTPUT nv_msgerror7072).
      END.
    END.
  END.
/*[BLANK]*/                      
  IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
    RUN PD_PUTError ("6").
/*[BLANK]*/                      
    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
    NO-ERROR.
    IF AVAILABLE IntPol7072 THEN DO:
      ASSIGN
      IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
      IntPol7072.GenSicBran     = NO
      IntPol7072.GenSicBranText = nv_msgerror7072
      IntPol7072.GenSicBranST   = "ERROR"
      IntPol7072.ErrorMessage   = nv_msgerror7072.
      IF IntPol7072.DocumentUID <> "" THEN
         IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
    END.
    RETURN.
  END.
/*[BLANK]*/                      
END.
/*[BLANK]*/                      
RUN PD_PUTError ("PD2").
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
NO-ERROR NO-WAIT.
IF AVAILABLE IntPol7072 THEN DO:
  ASSIGN
  IntPol7072.GenSicBran   = YES
  IntPol7072.GenSicBranBy = "WRSBQ7072.W"
  IntPol7072.GenSicBranDt = TODAY
  IntPol7072.GenSicBranTime = STRING(TIME,"HH:MM:SS")
  IntPol7072.GenSicBranText = ""
  IntPol7072.GenSicBranST = "".
/*[BLANK]*/                      
  IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN IntPol7072.TransferToPremium = YES. /*kridtiya i.*/
/*[BLANK]*/                      
  IF nv_msgerror7072 <> "" THEN DO:
    ASSIGN
    IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
    IntPol7072.GenSicBran   = NO
    IntPol7072.GenSicBranText = nv_msgerror7072
    IntPol7072.GenSicBranST = "ERROR"
    IntPol7072.ErrorMessage = nv_msgerror7072.
/*[BLANK]*/                      
    IF IntPol7072.DocumentUID <> "" THEN
       IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
  END.
  OUTPUT STREAM xmlstream TO PUT_WRSBQCuw3.TXT.
  PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
  OUTPUT STREAM xmlstream CLOSE.
/*[BLANK]*/                      
  RUN WRS\WRSBQCuw3.P (nv_RecIntPol7072). /*à¡çº·Ø¹ àºÕéÂ ÍÒ¡Ã áÊµÁ»ì µÍº¡ÅÑº*/
  OUTPUT STREAM xmlstream TO PUT_SAVEPD1IntS.TXT.
  PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
  OUTPUT STREAM xmlstream CLOSE.
  RUN PD_SAVEPD1IntS (nv_rec_rq, nv_RecIntPol7072). /*à¡çº·Ø¹ àºÕéÂ ÍÒ¡Ã áÊµÁ»ì µÍº¡ÅÑº*/
END.
/*[BLANK]*/                      
/*[BLANK]*/                      
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
  IF IntPol7072.ConfirmBy <> "AUTO" THEN DO:
/*[COMMENT]*/                        /*»2.1 2.2 ·Ø¹µèÓ¡ÇèÒ1áÊ¹ÍÍ¡¡ÃÁ¸ÃÃÁì D ËÃ×ÍàÅ¢ÊÒ¢Ò 2 ËÅÑ¡*/
    IF (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
       AND  nv_okprn = YES
    THEN DO:
/*[COMMENT]*/                          /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                         /* IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN /*Add kridtiya i. 70+72*/
/*[COMMENT]*/                            RUN PD_SAVEPD1FileAttach2 (nv_RecIntPol7072
/*[COMMENT]*/                                                   ,nv_rec_rq ).
      ELSE *//*Kridtiya i. */
        IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN DO:
            IF   IntPol7072.SERVICE_ID = "online" AND IntPol7072.CompanyCode = "834" THEN  
                RUN PD_SAVEPD1FileAtt834 (nv_RecIntPol7072
                                          ,nv_rec_rq ).
            ELSE 
                RUN PD_SAVEPD1FileAttach (nv_RecIntPol7072
                                          ,nv_rec_rq ).
        END.
    END.
    ELSE DO:
        IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN  
            RUN PD_SAVEPD1ChkVehicle. /*Êè§µÃÇ¨ÊÀÒ¾Ã¶*/
/*[BLANK]*/                      
    END. 
  END. /*IF IntS7072.ConfirmBy <> "AUTO" */
/*[BLANK]*/                      
  IF IntPol7072.ConfirmBy = "AUTO" THEN DO:
/*[BLANK]*/                      
    IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" AND IntPol7072.SERVICE_ID <> "PAYMENT" THEN 
       RUN WRS\WRSGwCtx 
               (IntPol7072.PolicyNumber
               ,IntPol7072.Rencnt
               ,IntPol7072.Endcnt
               ,0  /*RECID(uwm100)*/
               ,IntPol7072.CompanyCode  /*833*/
               ,IntPol7072.PolicyTypeCd /*2.2*/
               ,IntPol7072.RateGroup).  /*110*/ /*kridtiya i.*/
/*[BLANK]*/                      
    ELSE DO: /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          OUTPUT TO EXPIRT-ERROR.TXT APPEND.
/*[COMMENT]*/                          PUT "7. EXPIRT " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                              TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                              " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                          OUTPUT CLOSE.
/*[COMMENT]*/                          */
      IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.CompanyCode = "834" AND IntPol7072.SERVICE_ID <> "PAYMENT" THEN 
          RUN WRS\WRSGwCtx 
               (IntPol7072.PolicyNumber
               ,IntPol7072.Rencnt
               ,IntPol7072.Endcnt
               ,0  /*RECID(uwm100)*/
               ,IntPol7072.CompanyCode  /*833*/
               ,IntPol7072.PolicyTypeCd /*2.2*/
               ,IntPol7072.RateGroup).
      ELSE IF   IntPol7072.SERVICE_ID = "online" AND IntPol7072.CompanyCode = "834" THEN  
          RUN PD_SAVEPD1FileAtt834 (nv_RecIntPol7072
                                    ,nv_rec_rq ).
      ELSE DO:
          IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN DO:
              RUN PD_SAVEPD1FileAttach
                  (nv_RecIntPol7072
                   ,nv_rec_rq ).  /*RECID(IntS7072)*/
          END.
      END.
    END.
  END.
END.  /*IF AVAILABLE IntPol7072 */
OUTPUT TO PD_SavePD1_CK.TXT APPEND.
  PUT "nv_Renew :" nv_Renew IntPol7072.PolicyNumber FORMAT "X(18)"
      TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
      "nv_msgerror7072 : " nv_msgerror7072  FORMAT "X(150)" SKIP
      "service " IntPol7072.SERVICE_ID  FORMAT "X(15)" SKIP
      "Auto :"   IntPol7072.ConfirmBy   FORMAT "X(15)" SKIP
      " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
  OUTPUT CLOSE.
/*[BLANK]*/                      
RELEASE IntS7072.
RELEASE IntPol7072.
RELEASE IntQPolicy.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD17072 C-Win 
PROCEDURE PD_SAVEPD17072 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /* 20/03/2016
/*[COMMENT]*/                    nv_octets = "".
/*[COMMENT]*/                    FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70  /*70,72*/
/*[COMMENT]*/                        NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        RUN WRS/WRSDigit.p (output nv_octets).
/*[COMMENT]*/                        CREATE IntPol7072.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    ELSE nv_octets = IntPol7072.RqUID.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72  /*72*/
/*[COMMENT]*/                        NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                        nv_octets = "".
/*[BLANK]*/                      
/*[COMMENT]*/                        RUN WRS/WRSDigit.p (output nv_octets).
/*[COMMENT]*/                        CREATE IntPol7072.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    ELSE nv_octets = IntPol7072.RqUID.
/*[COMMENT]*/                         */
/*[COMMENT]*/                    ---- */
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1ChkFile C-Win 
PROCEDURE PD_SAVEPD1ChkFile :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:           RUN PD_CheckFpdf (INPUT nv_NameCompCd
/*[COMMENT]*/                                         ,INPUT "V72"
/*[COMMENT]*/                                         ,INPUT IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                                         ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
DEFINE INPUT  PARAMETER nv_NameCompCd        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER nv_PolicyType        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER nv_PolicyTypeCd      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER nv_SAVEmsgerror      AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_LineSeqno     AS INTEGER   NO-UNDO.
DEFINE VARIABLE NV_Lwaitcount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE NV_LcountAgain   AS INTEGER   NO-UNDO.
DEFINE VARIABLE NV_Lcount        AS INTEGER   NO-UNDO.
DEFINE VARIABLE NV_StartCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE NV_LastCount     AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_SearchDrive      AS CHARACTER NO-UNDO. /*"C:\"*/
DEFINE VARIABLE nv_SearchName       AS CHARACTER NO-UNDO. /*"AcroRd32.exe"*/
DEFINE VARIABLE nv_CreateFileAcroRd AS LOGICAL   NO-UNDO. /*YES/NO*/
DEFINE VARIABLE nv_CheckFile        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_savefile         AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE nv_savesize         AS DECIMAL   INITIAL 0       NO-UNDO. /*¢¹Ò´ file ·Õèä´é*/
DEFINE VARIABLE nv_firstsize        AS DECIMAL   FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0 NO-UNDO. /*¢¹Ò´ file ·Õè¾º¤ÃÑé§áÃ¡*/
DEFINE VARIABLE nv_CheckCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_CheckConfirm     AS LOGICAL   NO-UNDO.
/*[BLANK]*/                      
DEFINE VARIABLE nv_WaitCount        AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_WaitTotal        AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_CountChkFile     AS INTEGER   NO-UNDO.
DEFINE VARIABLE nv_ERRORCount       AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
nv_INPUTFileName = "".
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    nv_COPYTOFILE    = "".*/
/*[BLANK]*/                      
FOR EACH FNameAttach WHERE
         FNameAttach.CompanyCode  = nv_NameCompCd
     AND FNameAttach.PolicyTypeCd = nv_PolicyType   /*"V72"*/
     AND FNameAttach.CoverTypeCd  = nv_PolicyTypeCd /*IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/*/
     AND FNameAttach.EffDate     <= TODAY
NO-LOCK
BREAK BY FNameAttach.SelectNumber
:
  IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                   ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                      /**/
  LEAVE.
END.
IF nv_INPUTFileName = "" /* OR nv_COPYTOFILE = ""*/ THEN RETURN.
/*[COMMENT]*/                    /* --------------------------------------------------------- */ 
/*[BLANK]*/                      
ASSIGN
nv_SearchDrive       = "D:"
/*[COMMENT]*/                    /*
nv_SearchName        = "TESTCHECKFILE.txt"  /*"sombatp.PDF" */ */
nv_SearchName        = nv_INPUTFileName
nv_CreateFileAcroRd  = yes
nv_CheckFile         = ""
nv_firstsize         = 0
/*[BLANK]*/                      
nv_CheckCount   = 0
nv_CheckConfirm = NO
/*[COMMENT]*/                    /**/
nv_CountChkFile = 0
nv_WaitCount    = 0
nv_WaitTotal    = 1000000.  /* 1/2 ÇÔ¹Ò·Õ*/ 
/*[COMMENT]*/                    /*nv_WaitTotal    = 850000. /* 1/2 ÇÔ¹Ò·Õ*/*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    OUTPUT TO WRSChkFile.txt.
/*[COMMENT]*/                    PUT nv_CountChkFile " Filename: " nv_SearchName FORMAT "X(16)" 
/*[COMMENT]*/                        " nv_NameCompCd: " nv_NameCompCd  FORMAT "X(20)"
/*[COMMENT]*/                        " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    SKIP.
/*[COMMENT]*/                    OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[BLANK]*/                      
loop_ChkFile:
REPEAT:
/*[BLANK]*/                      
  nv_WaitCount    = 0.
  DO WHILE nv_WaitCount <= nv_WaitTotal: /* 1/2 ÇÔ¹Ò·Õ*/
    nv_WaitCount = nv_WaitCount + 1.
  END.
/*[BLANK]*/                      
  nv_savefile = "".
  nv_savesize = 0.
/*[BLANK]*/                      
  RUN WRS\WRSChkFile.P 
    (
     INPUT        nv_SearchDrive
    ,INPUT        nv_SearchName  
    ,INPUT        YES /*nv_CreateFileAcroRd*/
    ,INPUT-OUTPUT nv_CheckFile
    ,INPUT-OUTPUT nv_savefile
    ,INPUT-OUTPUT nv_savesize
    ).
/*[COMMENT]*/                      /* ---
/*[COMMENT]*/                      DISPLAY nv_SearchDrive     
/*[COMMENT]*/                            nv_SearchName      
/*[COMMENT]*/                            nv_CreateFileAcroRd
/*[COMMENT]*/                            nv_CheckFile
/*[COMMENT]*/                            nv_savefile
/*[COMMENT]*/                            nv_savesize
/*[COMMENT]*/                      WITH FRAME AA 1 COLUMN.
/*[COMMENT]*/                      PAUSE 0.
/*[COMMENT]*/                      -- */
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      FOR EACH ctemp NO-LOCK:
/*[COMMENT]*/                       display
/*[COMMENT]*/                       ctemp.Dirtext    format "x(60)".
/*[COMMENT]*/                       PAUSE 0.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      */
/*[COMMENT]*/                      /*****
/*[COMMENT]*/                      nv_savefile = "".
/*[COMMENT]*/                      nv_savesize = 0.
/*[BLANK]*/                      
/*[COMMENT]*/                      FOR EACH cdir NO-LOCK:
/*[COMMENT]*/                       display
/*[COMMENT]*/                       cdir.DirName     format "x(60)"
/*[COMMENT]*/                       cdir.FilNAME     format "x(60)"
/*[COMMENT]*/                       cdir.FilSize
/*[COMMENT]*/                       cdir.UseFileName format "x(60)" WITH FRAME BB.
/*[COMMENT]*/                       PAUSE 0.
/*[BLANK]*/                      
/*[COMMENT]*/                       nv_savefile = cdir.FilNAME .
/*[COMMENT]*/                       nv_savesize = cdir.FilSize .
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ******/
/*[COMMENT]*/                      /* ------------------------------------------- */
/*[COMMENT]*/                      /*¹Ñº¨Ó¹Ç¹à¢éÒàªç¤ file*/
  nv_CountChkFile = nv_CountChkFile + 1.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO WRSChkFile.txt APPEND.
/*[COMMENT]*/                      PUT nv_CountChkFile nv_CheckCount " Filename: " nv_SearchName FORMAT "X(16)" " size file: " nv_savesize
/*[COMMENT]*/                          " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.*/
/*[COMMENT]*/                      /* ------------------------------------------- */
/*[BLANK]*/                      
  IF nv_savefile = "" THEN DO: 
    nv_ERRORCount = nv_ERRORCount + 1.
/*[BLANK]*/                      
    IF nv_ERRORCount >= 20 THEN DO:
/*[BLANK]*/                      
      nv_savefile = "ERROR".
      LEAVE loop_ChkFile.
    END.
/*[BLANK]*/                      
    NEXT loop_ChkFile.
  END.
/*[BLANK]*/                      
  IF nv_savefile = nv_SearchName THEN DO: /*ª×èÍfile·Õè¤é¹ËÒä´é à·ÕèÂº¡Ñº ª×èÍ file ·ÕèËÒ*/
/*[BLANK]*/                      
    IF nv_savesize = 0 THEN NEXT loop_ChkFile.
/*[BLANK]*/                      
    IF nv_savesize <> 0 THEN DO:
/*[BLANK]*/                      
      nv_CheckCount = nv_CheckCount + 1. /*¹Ñº¡ÒÃä´é¢¹Ò´ file ÁÒ¡¡ÇèÒ 0*/
/*[BLANK]*/                      
      IF nv_firstsize <> nv_savesize THEN nv_firstsize = nv_savesize.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*       nv_WaitTotal    = 850000. /* äÁè¶Ö§ 1/2 ÇÔ¹Ò·Õ*/ */
       nv_WaitTotal    = 300000. /* äÁè¶Ö§ 1/2 ÇÔ¹Ò·Õ*/
    END.
/*[BLANK]*/                      
    IF nv_CheckCount >= 2 THEN DO: /*¤é¹¤ÃÑé§·Õè 2 à¾×èÍ¤ÇÒÁá¹èã¨ÇèÒ ¢¹Ò´ file à·èÒà´ÔÁ*/
/*[COMMENT]*/                        /*IF nv_CheckCount >= 1 THEN DO:   /* Test by kridtiya i. 30/05/2017*/*/
/*[BLANK]*/                      
      nv_CheckConfirm = YES.
/*[BLANK]*/                      
      IF nv_firstsize = nv_savesize THEN /*¢¹Ò´ file à·èÒà´ÔÁ*/
        LEAVE loop_ChkFile.
    END.
/*[BLANK]*/                      
    NEXT loop_ChkFile.
  END.
/*[BLANK]*/                      
  LEAVE loop_ChkFile.
END. /*loop_ChkFile*/
/*[BLANK]*/                      
PAUSE 2 NO-MESSAGE.   /*Add by Kridtiya i. wait certificate*/
/*[BLANK]*/                      
OUTPUT TO WRSChkFile.txt APPEND.
PUT nv_CountChkFile nv_CheckCount " Filename: " nv_SearchName FORMAT "X(16)" " size file: " nv_savesize
    " nv_NameCompCd: " nv_NameCompCd  FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
SKIP.
OUTPUT CLOSE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1ChkVehicle C-Win 
PROCEDURE PD_SAVEPD1ChkVehicle :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
      OUTPUT TO ConfirmBy.TXT.
      PUT IntPol7072.ConfirmBy SKIP.
      OUTPUT CLOSE.
/*[BLANK]*/                      
      IF IntPol7072.ChkVehicle = YES THEN DO:
        nv_resulttext = "Êè§µÃÇ¨ÊÀÒ¾Ã¶".
        nv_cvehtext   = "µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)
                      + " ¡ÃÁ¸ÃÃÁì¾Ãº.: "       + TRIM(nv_PolicyV72)
                      +   " ·ÐàºÕÂ¹Ã¶ : "       + TRIM(IntPol7072.Registration) 
                                    + " "       + TRIM(IntPol7072.RegisteredProvCd)
                      + " ¼ÙéàÍÒ»ÃÐ¡Ñ¹ÀÑÂ : "   + TRIM(TRIM(TRIM(IntPol7072.InsuredTitle)
                      + " " + TRIM(IntPol7072.InsuredName))
                      + " " + TRIM(IntPol7072.InsuredSurname)).
/*[BLANK]*/                      
        OUTPUT TO ChkVehicle.TXT.
        PUT " ConfirmBy:" IntPol7072.ConfirmBy  SKIP.
        PUT "ChkVehicle:" IntPol7072.ChkVehicle SKIP.
        PUT "           " nv_resulttext FORMAT "X(50)" SKIP.
        PUT "           " nv_cvehtext   FORMAT "X(200)" SKIP.
        PUT "ChkVehSend:" IntPol7072.ChkVehSend SKIP.
        PUT "   Mail to:" IntPol7072.ChkVehMail SKIP.
        PUT "------------------------------------------------" FORMAT "X(50)" SKIP.
        PUT "ChkVehAssignSend: " IntPol7072.ChkVehAssignSend SKIP.
        PUT "ChkVehAssignMail: " IntPol7072.ChkVehAssignMail SKIP.
        OUTPUT CLOSE.
/*[BLANK]*/                      
        IF IntPol7072.ChkVehSend = YES THEN DO:
          IF IntPol7072.ChkVehMail <> "" THEN DO:
            RUN GW/gwtomail 
              (IntPol7072.ChkVehMail /*To "USERNAME"*/
              ,""          /*CC.*/
              ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)) /*Subject: WARNING VIB*/
              ,nv_cvehtext /*Body*/
              ).
          END.
        END.
/*[BLANK]*/                      
        IF IntPol7072.ChkVehAssignSend = YES THEN DO:
/*[BLANK]*/                      
          IF IntPol7072.ChkVehAssignMail <> "" THEN DO:
/*[BLANK]*/                      
            RUN GW/gwtomail
              (IntPol7072.ChkVehAssignMail
              ,""
              ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70))
              ,nv_cvehtext
              ).
          END.
        END.
/*[BLANK]*/                      
        IF IntPol7072.TransferToPremium = YES THEN DO: /* Gen Uwm100 to DB GWCtx */
/*[BLANK]*/                      
          RUN WRS\WRSGwCtx 
              (IntPol7072.PolicyNumber
              ,IntPol7072.Rencnt
              ,IntPol7072.Endcnt
              ,0  /*RECID(uwm100)*/
              ,IntPol7072.CompanyCode  /*833*/
              ,IntPol7072.PolicyTypeCd /*2.2*/
              ,IntPol7072.RateGroup).  /*110*/
        END.
      END. /*IF IntS7072.ChkVehicle = YES*/
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt72 C-Win 
PROCEDURE PD_SAVEPD1FileAtt72 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt834 C-Win 
PROCEDURE PD_SAVEPD1FileAtt834 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_COPYTOFILE  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_LineSeqno   AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lwaitcount  AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LcountAgain AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lcount      AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_StartCount  AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LastCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Acno1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Agent  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_errort AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrgName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrmPrg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_firstchk AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_verror   AS CHARACTER NO-UNDO.
ASSIGN NV_Lwaitcount = 110000
NV_StartCount = 0
NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
FOR EACH TFileAttach: DELETE TFileAttach. END.
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
nv_LineSeqno = 0.
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/ /* 2.1             110 */
IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN 
    RUN PD_SAVEPD1FileAtt_1 (nv_RECIDIntPol7072
                            ,nv_RECIDIntS7072).
ELSE DO:  /*case »¡µÔ add by kridtiya i. */ 
IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
  RUN WSP/WSPMCpny.P
      (IntPol7072.CompanyCode
      ,IntPol7072.BranchCd
      ,"V70"
      ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
      ,OUTPUT nv_BrokerBranch 
      ,OUTPUT nv_Acno1
      ,OUTPUT nv_Agent
      ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
      (INPUT IntPol7072.CompanyCode
      ,INPUT "V70"      /*v70,v72*/
      ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                          /**/
      ,INPUT-OUTPUT nv_NameCompCd
      ,INPUT-OUTPUT nv_PrgName
      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
  IF    IntPol7072.PolicyTypeCd = "1"
     OR IntPol7072.PolicyTypeCd = "2"
     OR IntPol7072.PolicyTypeCd = "3"
  THEN DO:
      IF  (IntPol7072.PolicyTypeCd = "3"  AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
          (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )  /*TEST Prn cover 1*/
         OR IntPol7072.CompanyCode  = "476" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
      THEN DO:
        IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
        RUN VALUE(nv_PrgName)
          (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
          ,IntPol7072.PolicyNumber 
          ,IntPol7072.Rencnt       
          ,IntPol7072.Endcnt       
          ,IntPol7072.DocumentUID  
          ,IntPol7072.RqUID        /*nv_code keyRequestIndRq*/
          ,""                      /*n_user  */
          ,""                      /*n_passwd */
          ,OUTPUT nv_SAVEmsgerror).
        IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[COMMENT]*/                                /*RUN wctx\Wctxr702_RP.p*/
            RUN Wctx/Wctxr701_1_CP.P
                (IntPol7072.CompanyCode  
                 ,IntPol7072.PolicyNumber 
                 ,IntPol7072.Rencnt       
                 ,IntPol7072.Endcnt       
                 ,IntPol7072.DocumentUID  
                 ,IntPol7072.RqUID        
                 ,""                      
                 ,""                      
                 ,OUTPUT nv_SAVEmsgerror).            
        END.
      END.
  END.
  ELSE DO:  /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ *//*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
    IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
       OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" OR IntPol7072.CompanyCode = "570"
       OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
       OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" )             /* isuzu */
       AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") ) THEN DO:  /* ·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
      IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".  /*"Wctx/Wctxr703_1".*/
                         ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
      RUN VALUE(nv_PrgName)
         (IntPol7072.CompanyCode
         ,IntPol7072.PolicyNumber
         ,IntPol7072.Rencnt
         ,IntPol7072.Endcnt
         ,IntPol7072.DocumentUID 
         ,IntPol7072.RqUID
         ,""
         ,""
         ,OUTPUT nv_SAVEmsgerror).
      IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[COMMENT]*/                              /*RUN wctx\Wctxr702_RP.p*/
          RUN Wctx/Wctxr703_1ASN_RP.p
              (IntPol7072.CompanyCode  
              ,IntPol7072.PolicyNumber 
              ,IntPol7072.Rencnt       
              ,IntPol7072.Endcnt       
              ,IntPol7072.DocumentUID  
              ,IntPol7072.RqUID        
              ,""                      
              ,""                      
              ,OUTPUT nv_SAVEmsgerror).            
      END.
    END.
  END.
  IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
  FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
  nv_firstchk   = YES.
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V70"
      ,INPUT IntPol7072.PolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT". 
    OUTPUT TO VALUE(nv_verror).
    PUT 
    "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[COMMENT]*/                      /* ------------------------------------------------------------------------ */
/*[COMMENT]*/                    /*OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"  /*1234567890*/
/*[COMMENT]*/                      IntPol7072.CompanyCode 
/*[COMMENT]*/                      IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                      IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                      TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                      STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                      SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.  */
  IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = IntPol7072.PolicyTypeCd.
  FOR EACH FNameAttach WHERE FNameAttach.CompanyCode  = nv_NameCompCd
     AND FNameAttach.PolicyTypeCd = "V70"
     AND FNameAttach.CoverTypeCd  = nv_covcodtyp1  /*IntPol7072.PolicyTypeCd*/
     AND FNameAttach.EffDate     <= TODAY  NO-LOCK
  BREAK BY FNameAttach.SelectNumber:
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
    IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
/*[COMMENT]*/                        /*IF IntPol7072.CompanyCode = "469" THEN nv_COPYTOFILE = string(IntPol7072.DocumentUID) + "_" + nv_COPYTOFILE.*//*comment by Kridtiya i. 24/07/2017*/
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
    loop1:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
        NEXT loop1.
      END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                             nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
        DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
      END.
      RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT nv_RECIDIntPol7072 ).
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1   = TFileAttach.FileBinary
            IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntS7072.AttachFile1     = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2   = TFileAttach.FileBinary
            IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntS7072.AttachFile2     = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
      LEAVE loop1.
    END. /*loop1:*/
  END. /*FOR EACH FNameAttach*/
END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /* ------------------------------------------------------------------------ */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
  RUN WSP/WSPMCpny.P
      (IntPol7072.CompanyCode
      ,IntPol7072.BranchCd
      ,"V72"
      ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
      ,OUTPUT nv_BrokerBranch 
      ,OUTPUT nv_Acno1
      ,OUTPUT nv_Agent
      ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
  NV_StartCount = 0.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
     (INPUT        IntPol7072.CompanyCode
     ,INPUT        "V72"      /*v70,v72*/
     ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                         /**/
     ,INPUT-OUTPUT nv_NameCompCd
     ,INPUT-OUTPUT nv_PrgName
     ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**//*Blank form Compulsory */
  IF IntPol7072.CompanyCode = "833" THEN DO:
    RUN PD_ChkBlankForm
         (INPUT-OUTPUT nv_NameCompCd
         ,INPUT-OUTPUT nv_PrgName
         ,INPUT-OUTPUT nv_PrmPrg).
  END.
  IF nv_PrgName = "" THEN DO:
    RUN Wctx/wctxr702.P
       (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
       ,IntPol7072.CMIPolicyNumber
       ,IntPol7072.Rencnt
       ,IntPol7072.Endcnt
       ,IntPol7072.CMIDocumentUID
       ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
       ,""         /*n_user   */
       ,""         /*n_passwd */
       ,nv_PrmPrg  /*Name Report*/
       ,OUTPUT nv_SAVEmsgerror).
  END.
  ELSE DO:
    nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
    RUN VALUE(nv_PrgName)
        (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
        ,IntPol7072.CMIPolicyNumber
        ,IntPol7072.Rencnt
        ,IntPol7072.Endcnt
        ,IntPol7072.CMIDocumentUID
        ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
        ,""        /*n_user   */
        ,""        /*n_passwd */
        ,nv_PrmPrg /*Name Report="V72A4"*/
        ,""        /*remark*/
        ,OUTPUT nv_SAVEmsgerror).
  END.
  IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
      RUN wctx\Wctxr702_RP.p
       (IntPol7072.CompanyCode       /*nv_BrokerCompany*/
       ,IntPol7072.CMIPolicyNumber   
       ,IntPol7072.Rencnt            
       ,IntPol7072.Endcnt            
       ,IntPol7072.CMIDocumentUID    
       ,IntPol7072.RqUID             /*nv_code  keyRequestIndRq*/
       , ""                          /*n_user   */
       , ""                          /*n_passwd */
       , ""                          /*Name Report*/
       ,OUTPUT nv_SAVEmsgerror).    
  END.
  IF nv_SAVEmsgerror <> "" THEN RETURN.     
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.  */                     
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V72"
      ,INPUT IntPol7072.CMIPolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT".
    OUTPUT TO VALUE(nv_verror).
    PUT 
    "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "    IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: "  IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
  ASSIGN nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
  FOR EACH FNameAttach WHERE
           FNameAttach.CompanyCode  = nv_NameCompCd
       AND FNameAttach.PolicyTypeCd = "V72"
       AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd  /*¾Ãº ËÃ×Í "T"*/
       AND FNameAttach.EffDate     <= TODAY
  NO-LOCK
  BREAK BY FNameAttach.SelectNumber
  :
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.  /**/
    IF FNameAttach.ToFileName   = "" THEN
           nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
           nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
    IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
            nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[COMMENT]*/                        /**/
    loop2:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
        NEXT loop2.
      END.
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT nv_RECIDIntPol7072 ).
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1   = TFileAttach.FileBinary
            IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntS7072.AttachFile1     = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2   = TFileAttach.FileBinary
            IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntS7072.AttachFile2     = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END.
      LEAVE loop2.
    END. /*loop2:*/
  END. /*FOR EACH FNameAttach*/
END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
END. /*end kridtiya i.7072*/  /*..............*/
FOR EACH TFileAttach: DELETE TFileAttach. END.
RELEASE FNameAttach.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAttach C-Win 
PROCEDURE PD_SAVEPD1FileAttach :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_COPYTOFILE  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_LineSeqno   AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lwaitcount  AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LcountAgain AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lcount      AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_StartCount  AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LastCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Acno1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Agent  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_errort AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrgName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrmPrg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_firstchk AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_verror   AS CHARACTER NO-UNDO.
ASSIGN NV_Lwaitcount = 110000
NV_StartCount = 0
NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
FOR EACH TFileAttach: DELETE TFileAttach. END.
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
nv_LineSeqno = 0.
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/ /* 2.1             110 */
IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN
    RUN PD_SAVEPD1FileAtt_1 (nv_RECIDIntPol7072
                            ,nv_RECIDIntS7072).
ELSE DO:  /*case »¡µÔ add by kridtiya i. */ 
IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
  RUN WSP/WSPMCpny.P
      (IntPol7072.CompanyCode
      ,IntPol7072.BranchCd
      ,"V70"
      ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
      ,OUTPUT nv_BrokerBranch 
      ,OUTPUT nv_Acno1
      ,OUTPUT nv_Agent
      ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
      (INPUT IntPol7072.CompanyCode
      ,INPUT "V70"      /*v70,v72*/
      ,INPUT IntPol7072.PolicyTypeCd
      ,INPUT-OUTPUT nv_NameCompCd
      ,INPUT-OUTPUT nv_PrgName
      ,INPUT-OUTPUT nv_PrmPrg ).
  IF IntPol7072.CompanyCode  = "476" THEN 
    RUN PD_SAVEPD1F_PrgName (INPUT nv_RECIDIntPol7072
                            ,INPUT-OUTPUT nv_PrgName).
  IF    IntPol7072.PolicyTypeCd = "1" OR IntPol7072.PolicyTypeCd = "2" OR IntPol7072.PolicyTypeCd = "3" THEN DO:
/*[COMMENT]*/                        /* Add by Kridtiya i...Lockton */
    IF IntPol7072.CompanyCode = "469" THEN DO:   
      IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1". /*"Wctx/Wctxr708".*/
      ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr708.p Form 1 Policy*/
      RUN VALUE(nv_PrgName)        
         (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
         ,IntPol7072.PolicyNumber
         ,IntPol7072.Rencnt
         ,IntPol7072.Endcnt
         ,IntPol7072.DocumentUID
          ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
         ,""               /*n_user   */
         ,""               /*n_passwd */
         ,OUTPUT nv_SAVEmsgerror).
    END. /*End add Kridtiya i. */
    ELSE DO:
      IF  (IntPol7072.PolicyTypeCd = "3"  AND substr(IntPol7072.PolicyNumber,1,1) <> "R" AND substr(IntPol7072.PolicyNumber,1,1) <> "Q" ) OR  /*kridtiya i.*/
          (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )  /*TEST Prn cover 1*/
         OR IntPol7072.CompanyCode  = "476" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
      THEN DO:
        IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
        RUN VALUE(nv_PrgName)
          (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
          ,IntPol7072.PolicyNumber
          ,IntPol7072.Rencnt
          ,IntPol7072.Endcnt
          ,IntPol7072.DocumentUID
          ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
          ,""  /*n_user  */
          ,""  /*n_passwd */
          ,OUTPUT nv_SAVEmsgerror).
      END.
    END.
  END.
  ELSE DO:  /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ *//*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
    IF (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND 
        substr(IntPol7072.PolicyNumber,1,1) <> "R" AND substr(IntPol7072.PolicyNumber,1,1) <> "Q" )
       OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834"  OR IntPol7072.CompanyCode = "570" OR IntPol7072.CompanyCode = "1012" 
       OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"  OR IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "1098" 
       OR    IntPol7072.CompanyCode = "476" OR IntPol7072.CompanyCode = "210"  OR IntPol7072.CompanyCode = "M82" OR IntPol7072.CompanyCode = "1103" 
       OR    IntPol7072.CompanyCode = "M73" OR IntPol7072.CompanyCode = "M85"  OR IntPol7072.CompanyCode = "839" OR IntPol7072.CompanyCode = "1107"
       OR    IntPol7072.CompanyCode = "1018" OR    IntPol7072.CompanyCode = "1470" OR IntPol7072.CompanyCode = "1752"
       OR    IntPol7072.CompanyCode = "444" OR IntPol7072.CompanyCode = "1056" OR IntPol7072.CompanyCode = "1141" OR IntPol7072.CompanyCode = "1146"
       OR    IntPol7072.CompanyCode = "1554" OR    IntPol7072.CompanyCode = "1798" OR    IntPol7072.CompanyCode = "2117" )
       AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") ) THEN DO:  /* ·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
      IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".     /*"Wctx/Wctxr703_1".*/
                         ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr703A4*/
      RUN VALUE(nv_PrgName)
         (IntPol7072.CompanyCode
         ,IntPol7072.PolicyNumber
         ,IntPol7072.Rencnt
         ,IntPol7072.Endcnt
         ,IntPol7072.DocumentUID 
         ,IntPol7072.RqUID
         ,""
         ,""
         ,OUTPUT nv_SAVEmsgerror).
    END.
  END.
  IF nv_SAVEmsgerror <> "" THEN  RETURN.
  FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
  nv_firstchk   = YES.
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V70"
      ,INPUT IntPol7072.PolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT". 
    OUTPUT TO VALUE(nv_verror).
    PUT "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
  IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = IntPol7072.PolicyTypeCd.
  FOR EACH FNameAttach WHERE FNameAttach.CompanyCode  = nv_NameCompCd
     AND FNameAttach.PolicyTypeCd = "V70"
     AND FNameAttach.CoverTypeCd  = nv_covcodtyp1  /*IntPol7072.PolicyTypeCd*/
     /*AND FNameAttach.EffDate     <= TODAY*/     NO-LOCK
  BREAK BY FNameAttach.SelectNumber:
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
    IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
/*[COMMENT]*/                        /*IF IntPol7072.CompanyCode = "469" THEN nv_COPYTOFILE = string(IntPol7072.DocumentUID) + "_" + nv_COPYTOFILE.*//*comment by Kridtiya i. 24/07/2017*/
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
    loop1:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
        NEXT loop1.
      END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                             nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
        DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
      END.
      RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT RECID(IntPol7072) ).
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1   = TFileAttach.FileBinary
            IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntS7072.AttachFile1     = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2   = TFileAttach.FileBinary
            IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntS7072.AttachFile2     = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
      LEAVE loop1.
    END. /*loop1:*/
  END. /*FOR EACH FNameAttach*/
END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /* ------------------------------------------------------------------------ */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
  RUN WSP/WSPMCpny.P
      (IntPol7072.CompanyCode
      ,IntPol7072.BranchCd
      ,"V72"
      ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
      ,OUTPUT nv_BrokerBranch 
      ,OUTPUT nv_Acno1
      ,OUTPUT nv_Agent
      ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
  NV_StartCount = 0.
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
     (INPUT        IntPol7072.CompanyCode
     ,INPUT        "V72"      /*v70,v72*/
     ,INPUT        IntPol7072.CMIPolicyTypeCd
     ,INPUT-OUTPUT nv_NameCompCd
     ,INPUT-OUTPUT nv_PrgName
     ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**//*Blank form Compulsory */
  IF IntPol7072.CompanyCode = "833" THEN DO:
    RUN PD_ChkBlankForm
         (INPUT-OUTPUT nv_NameCompCd
         ,INPUT-OUTPUT nv_PrgName
         ,INPUT-OUTPUT nv_PrmPrg).
  END.
  IF nv_PrgName = "" THEN DO:
    RUN Wctx/wctxr702.P
       (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
       ,IntPol7072.CMIPolicyNumber
       ,IntPol7072.Rencnt
       ,IntPol7072.Endcnt
       ,IntPol7072.CMIDocumentUID
       ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
       ,""         /*n_user   */
       ,""         /*n_passwd */
       ,nv_PrmPrg  /*Name Report*/
       ,OUTPUT nv_SAVEmsgerror).
  END.
  ELSE DO:
    nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
    RUN VALUE(nv_PrgName)
        (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
        ,IntPol7072.CMIPolicyNumber
        ,IntPol7072.Rencnt
        ,IntPol7072.Endcnt
        ,IntPol7072.CMIDocumentUID
        ,IntPol7072.CMIBarCodeNumber
        ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
        ,""        /*n_user   */
        ,""        /*n_passwd */
        ,nv_PrmPrg /*Name Report="V72A4"*/
        ,""        /*remark*/
        ,OUTPUT nv_SAVEmsgerror).
  END.
  IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
      RUN wctx\Wctxr702_RP.p
       (IntPol7072.CompanyCode       /*nv_BrokerCompany*/
       ,IntPol7072.CMIPolicyNumber   
       ,IntPol7072.Rencnt            
       ,IntPol7072.Endcnt            
       ,IntPol7072.CMIDocumentUID    
       ,IntPol7072.RqUID             /*nv_code  keyRequestIndRq*/
       , ""                          /*n_user   */
       , ""                          /*n_passwd */
       , ""                          /*Name Report*/
       ,OUTPUT nv_SAVEmsgerror).    
  END.
  IF nv_SAVEmsgerror <> "" THEN RETURN.     
  PAUSE 1 NO-MESSAGE.                       
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V72"
      ,INPUT IntPol7072.CMIPolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT".
    OUTPUT TO VALUE(nv_verror).
    PUT "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "    IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: "  IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
  ASSIGN nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
  FOR EACH FNameAttach WHERE
           FNameAttach.CompanyCode  = nv_NameCompCd
       AND FNameAttach.PolicyTypeCd = "V72"
       AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd  /*¾Ãº ËÃ×Í "T"*/
/*[COMMENT]*/                           /*AND FNameAttach.EffDate     <= TODAY*/
  NO-LOCK
  BREAK BY FNameAttach.SelectNumber  :
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.  /**/
    IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
           nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
    IF TRIM(nv_COPYTOFILE) = ".PDF" THEN nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
    loop2:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
        NEXT loop2.
      END.
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT RECID(IntPol7072) ).
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1   = TFileAttach.FileBinary
            IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntS7072.AttachFile1     = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2   = TFileAttach.FileBinary
            IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntS7072.AttachFile2     = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END.
      LEAVE loop2.
    END. /*loop2:*/
  END. /*FOR EACH FNameAttach*/
END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
END. /*end kridtiya i.7072*/  /*..............*/
FOR EACH TFileAttach: DELETE TFileAttach. END.
RELEASE FNameAttach.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAttach2 C-Win 
PROCEDURE PD_SAVEPD1FileAttach2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
ASSIGN
NV_Lwaitcount = 110000
NV_StartCount = 0
NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
FOR EACH TFileAttach: DELETE TFileAttach. END.
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
nv_LineSeqno = 0.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/
/*[COMMENT]*/                       /* 2.1                             110 */
IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
  RUN WSP/WSPMCpny.P
       (IntPol7072.CompanyCode
       ,IntPol7072.BranchCd
       ,"V70"
       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
       ,OUTPUT nv_BrokerBranch 
       ,OUTPUT nv_Acno1
       ,OUTPUT nv_Agent
       ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
      (INPUT IntPol7072.CompanyCode
      ,INPUT "V70"      /*v70,v72*/
      ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                          /**/
      ,INPUT-OUTPUT nv_NameCompCd
      ,INPUT-OUTPUT nv_PrgName
      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
  IF    IntPol7072.PolicyTypeCd = "1"
     OR IntPol7072.PolicyTypeCd = "2"
     OR IntPol7072.PolicyTypeCd = "3"
  THEN DO:
      IF  (IntPol7072.PolicyTypeCd = "3"   AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
/*[COMMENT]*/                              /*TEST Prn cover 1*/
         (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )
/*[COMMENT]*/                              /*Isuzu */
/*[COMMENT]*/                          /*OR (IntPol7072.CompanyCode  = "476" AND IntPol7072.PolicyTypeCd = "1" )*/
      OR (IntPol7072.CompanyCode  = "476" ) OR (IntPol7072.CompanyCode  = "839") /*Add ART*/
      THEN DO:
/*[BLANK]*/                      
        IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[BLANK]*/                      
        RUN VALUE(nv_PrgName)
           (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
           ,IntPol7072.PolicyNumber
           ,IntPol7072.Rencnt
           ,IntPol7072.Endcnt
           ,IntPol7072.DocumentUID
           ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
           ,""               /*n_user  */
           ,""               /*n_passwd */
           ,OUTPUT nv_SAVEmsgerror).
      END.
  END.
  ELSE DO:
/*[COMMENT]*/                        /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                        /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
    IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
       OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
       OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"
       OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" ) /*isuzu*/
/*[COMMENT]*/                               /**/
       AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") )
/*[COMMENT]*/                                /*·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
    THEN DO:
/*[BLANK]*/                      
      IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".  /*"Wctx/Wctxr703_1".*/
                         ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
/*[BLANK]*/                      
      RUN VALUE(nv_PrgName)
          (IntPol7072.CompanyCode
          ,IntPol7072.PolicyNumber
          ,IntPol7072.Rencnt
          ,IntPol7072.Endcnt
          ,IntPol7072.DocumentUID 
          ,IntPol7072.RqUID
          ,""
          ,""
          ,OUTPUT nv_SAVEmsgerror).
    END.
  END.
  IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
  FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
  NO-ERROR NO-WAIT.
  nv_firstchk   = YES.
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
/*[BLANK]*/                      
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V70"
      ,INPUT IntPol7072.PolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT".
/*[BLANK]*/                      
    OUTPUT TO VALUE(nv_verror).
    PUT 
    "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
  FOR EACH FNameAttach WHERE
           FNameAttach.CompanyCode  = nv_NameCompCd
       AND FNameAttach.PolicyTypeCd = "V70"
       AND FNameAttach.CoverTypeCd  = IntPol7072.PolicyTypeCd
       AND FNameAttach.EffDate     <= TODAY
  NO-LOCK
  BREAK BY FNameAttach.SelectNumber:
/*[BLANK]*/                      
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
    IF FNameAttach.ToFileName   = "" THEN
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[BLANK]*/                      
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[BLANK]*/                      
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
    loop1:
    REPEAT:
/*[BLANK]*/                      
      IF nv_firstchk = NO THEN DO:   
/*[BLANK]*/                      
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
        NEXT loop1.
      END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                          nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[BLANK]*/                      
      IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[BLANK]*/                      
        DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
      END.
/*[BLANK]*/                      
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[BLANK]*/                      
        FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[BLANK]*/                      
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[BLANK]*/                        
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
/*[BLANK]*/                      
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
        NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1     = TFileAttach.FileBinary
            IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
            IntS7072.AttachFile1       = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2     = TFileAttach.FileBinary
            IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
            IntS7072.AttachFile2       = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN
            IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[BLANK]*/                      
      LEAVE loop1.
    END. /*loop1:*/
  END. /*FOR EACH FNameAttach*/
/*[BLANK]*/                      
END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /*  -------------------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[BLANK]*/                      
  RUN WSP/WSPMCpny.P
       (IntPol7072.CompanyCode
       ,IntPol7072.BranchCd
       ,"V72"
       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
       ,OUTPUT nv_BrokerBranch 
       ,OUTPUT nv_Acno1
       ,OUTPUT nv_Agent
       ,OUTPUT nv_errort).
/*[BLANK]*/                      
  IF nv_errort <> "" THEN RETURN.
/*[BLANK]*/                      
  NV_StartCount = 0.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
      (INPUT        IntPol7072.CompanyCode
      ,INPUT        "V72"      /*v70,v72*/
      ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                          /**/
      ,INPUT-OUTPUT nv_NameCompCd
      ,INPUT-OUTPUT nv_PrgName
      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
/*[BLANK]*/                      
/*[COMMENT]*/                      /*Blank form Compulsory */
  IF IntPol7072.CompanyCode = "833" THEN DO:
/*[BLANK]*/                      
    RUN PD_ChkBlankForm
         (INPUT-OUTPUT nv_NameCompCd
         ,INPUT-OUTPUT nv_PrgName
         ,INPUT-OUTPUT nv_PrmPrg).
  END.
/*[BLANK]*/                      
  IF nv_PrgName = "" THEN DO:
/*[BLANK]*/                      
    RUN Wctx/wctxr702.P
         (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
         ,IntPol7072.CMIPolicyNumber
         ,IntPol7072.Rencnt
         ,IntPol7072.Endcnt
         ,IntPol7072.CMIDocumentUID
         ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
         ,""         /*n_user   */
         ,""         /*n_passwd */
         ,nv_PrmPrg  /*Name Report*/
         ,OUTPUT nv_SAVEmsgerror).
  END.
  ELSE DO:
    nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*
/*[COMMENT]*/                        RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
    RUN VALUE(nv_PrgName)
         (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
         ,IntPol7072.CMIPolicyNumber
         ,IntPol7072.Rencnt
         ,IntPol7072.Endcnt
         ,IntPol7072.CMIDocumentUID
         ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
         ,""         /*n_user   */
         ,""         /*n_passwd */
         ,nv_PrmPrg  /*Name Report="V72A4"*/
         ,""         /*remark*/
         ,OUTPUT nv_SAVEmsgerror).
  END.
  IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "2. ChkPDF:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[BLANK]*/                      
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V72"
      ,INPUT IntPol7072.CMIPolicyTypeCd
      ,OUTPUT nv_SAVEmsgerror).
/*[BLANK]*/                      
  IF nv_SAVEmsgerror <> "" THEN DO:
/*[BLANK]*/                      
    nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT".
/*[BLANK]*/                      
    OUTPUT TO VALUE(nv_verror).
    PUT 
    "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
/*[BLANK]*/                      
    RETURN. 
  END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[BLANK]*/                      
  ASSIGN
  nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[BLANK]*/                      
  FOR EACH FNameAttach WHERE
           FNameAttach.CompanyCode  = nv_NameCompCd
       AND FNameAttach.PolicyTypeCd = "V72"
       AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
       AND FNameAttach.EffDate     <= TODAY
  NO-LOCK
  BREAK BY FNameAttach.SelectNumber
  :
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
    IF FNameAttach.ToFileName   = "" THEN
           nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[BLANK]*/                      
           nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[BLANK]*/                      
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[BLANK]*/                      
    IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
            nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[COMMENT]*/                        /**/
    loop2:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
        NEXT loop2.
      END.
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
        NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1     = TFileAttach.FileBinary
            IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
            IntS7072.AttachFile1       = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2     = TFileAttach.FileBinary
            IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
            IntS7072.AttachFile2       = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN
            IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END.
      LEAVE loop2.
    END. /*loop2:*/
  END. /*FOR EACH FNameAttach*/
END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[BLANK]*/                      
FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[BLANK]*/                      
RELEASE FNameAttach.
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt_1 C-Win 
PROCEDURE PD_SAVEPD1FileAtt_1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO. 
DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO. 
DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.*/
DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO. 
DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                          /*Add kridtiya i. Date. 20/09/2016*/
IF   substr(IntPol7072.PolicyNumber,1,1) = "R" OR  substr(IntPol7072.PolicyNumber,1,1) = "Q"  THEN DO:  /* CMI*/
/*[COMMENT]*/                        /* by Kridtiya i. 20/09/2016 */
    RUN WSP/WSPMCpny.P
       (IntPol7072.CompanyCode
       ,IntPol7072.BranchCd
       ,"V70"
       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
       ,OUTPUT nv_BrokerBranch 
       ,OUTPUT nv_Acno1
       ,OUTPUT nv_Agent
       ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
  IF nv_PrgName = "" THEN DO:
      RUN Wctx/wctxr702.P
          (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
           ,IntPol7072.CMIPolicyNumber
           ,IntPol7072.Rencnt
           ,IntPol7072.Endcnt
           ,IntPol7072.CMIDocumentUID
           ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
           ,""         /*n_user   */
           ,""         /*n_passwd */
           ,nv_PrmPrg  /*Name Report*/
           ,OUTPUT nv_SAVEmsgerror).
  END.
  ELSE DO:
      nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                          /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
      RUN VALUE(nv_PrgName)
          (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
           ,IntPol7072.CMIPolicyNumber
           ,IntPol7072.Rencnt
           ,IntPol7072.Endcnt
           ,IntPol7072.CMIDocumentUID
           ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
           ,""        /*n_user   */
           ,""        /*n_passwd */
           ,nv_PrmPrg /*Name Report="V72A4"*/
           ,OUTPUT nv_SAVEmsgerror).
  END.
END.
ELSE DO:
/*[BLANK]*/                      
  RUN WSP/WSPMCpny.P
       (IntPol7072.CompanyCode
       ,IntPol7072.BranchCd
       ,"V70"
       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
       ,OUTPUT nv_BrokerBranch 
       ,OUTPUT nv_Acno1
       ,OUTPUT nv_Agent
       ,OUTPUT nv_errort).
  IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
  ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
  RUN PD_FNameAttach
      (INPUT IntPol7072.CompanyCode
      ,INPUT "V70"      /*v70,v72*/
      ,INPUT (IntPol7072.PolicyTypeCd + "T")
/*[COMMENT]*/                          /**/
      ,INPUT-OUTPUT nv_NameCompCd
      ,INPUT-OUTPUT nv_PrgName
      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
  IF IntPol7072.CompanyCode  = "476" THEN 
            RUN PD_SAVEPD1F_PrgName (INPUT nv_RECIDIntPol7072
                                    ,INPUT-OUTPUT nv_PrgName).
/*[BLANK]*/                      
  IF    IntPol7072.PolicyTypeCd = "1" OR IntPol7072.PolicyTypeCd = "2" OR IntPol7072.PolicyTypeCd = "3" THEN DO:
      DEFINE VARIABLE nv_code       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE n_err         AS CHARACTER NO-UNDO.
      ASSIGN
          nv_code      = STRING(TODAY,"99/99/9999")
                       + STRING(TIME,"HH:MM:SS")
                       + SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
          n_err        = ""
          nv_PrmPrg    = "V70V72A4Receipt".
/*[BLANK]*/                           
       IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr707A4_3P1".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[COMMENT]*/                                                    /*
/*[COMMENT]*/                           IF (IntPol7072.RateGroup = "420" AND IntPol7072.GrossVehOrCombinedWeight <= "3" ) THEN nv_PrgName = "Wctx/" + "wctxizu706".
/*[COMMENT]*/                           ELSE IF (IntPol7072.RateGroup = "110" AND IntPol7072.SeatingCapacity  <= "15" )   THEN nv_PrgName = "Wctx/" + "wctxizu706".*/
/*[BLANK]*/                      
        RUN VALUE(nv_PrgName)
                             (IntPol7072.CompanyCode                    
                             ,IntPol7072.PolicyNumber                 
                             ,IntPol7072.Rencnt /*INTEGER*/           
                             ,IntPol7072.Endcnt /*INTEGER*/           
                             ,IntPol7072.DocumentUID                  
                             ,IntPol7072.RqUID                                
                             ,""                                       
                             ,""                             
                             ,IntPol7072.CMIPolicyNumber               
                             ,IntPol7072.CMIDocumentUID                
                             ,"0" /*CHARACTER*/                  
                             ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
                             ,OUTPUT nv_SAVEmsgerror).
  END.
  ELSE DO:
/*[COMMENT]*/                        /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                        /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐÀÑÂ + ¾Ãº.  » 2.1 3.1 */
    IF  (IntPol7072.PolicyTypeCd = "2.1") OR (IntPol7072.PolicyTypeCd = "3.1")  THEN DO:
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr703_1".
                         ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/*/
/*[BLANK]*/                      
        IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr709A4_3P2".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[BLANK]*/                              
        RUN VALUE(nv_PrgName)
                             (IntPol7072.CompanyCode                  
                             ,IntPol7072.PolicyNumber                 
                             ,IntPol7072.Rencnt /*INTEGER*/           
                             ,IntPol7072.Endcnt /*INTEGER*/           
                             ,IntPol7072.DocumentUID                  
                             ,IntPol7072.RqUID                                
                             ,""                                       
                             ,""                                       
/*[COMMENT]*/                                                 /* , nv_PrmPrg        /*Name Report*/*/                
                             ,IntPol7072.CMIPolicyNumber               
                             ,IntPol7072.CMIDocumentUID                
                             ,"0" /*CHARACTER*/                  
                             ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
                             ,OUTPUT nv_SAVEmsgerror).
/*[BLANK]*/                              
    END.
    ELSE DO:   /*  2.2  3.2 */
        IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr712A4_3P3".
                           ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[BLANK]*/                      
        RUN VALUE(nv_PrgName)
                             (IntPol7072.CompanyCode                  
                             ,IntPol7072.PolicyNumber                 
                             ,IntPol7072.Rencnt /*INTEGER*/           
                             ,IntPol7072.Endcnt /*INTEGER*/           
                             ,IntPol7072.DocumentUID                  
                             ,IntPol7072.RqUID                                
                             ,""                                       
                             ,""                                       
/*[COMMENT]*/                                                 /* , nv_PrmPrg        /*Name Report*/*/                
                             ,IntPol7072.CMIPolicyNumber               
                             ,IntPol7072.CMIDocumentUID                
                             ,"0" /*CHARACTER*/                  
                             ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
                             ,OUTPUT nv_SAVEmsgerror).   
/*[BLANK]*/                      
    END.
  END.
END.  /* by Kridtiya i. 20/09/2016 */
  IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ---------------------------------------------------- */
  FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
  NO-ERROR NO-WAIT.
  nv_firstchk   = YES.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "2. ChkPDF:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
  RUN PD_SAVEPD1ChkFile
      (INPUT nv_NameCompCd
      ,INPUT "V70"
      ,INPUT (IntPol7072.PolicyTypeCd + "T")
      ,OUTPUT nv_SAVEmsgerror).
  IF nv_SAVEmsgerror <> "" THEN DO:
    nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
              + ".TXT".
/*[BLANK]*/                      
    OUTPUT TO VALUE(nv_verror).
    PUT 
    "Not found file pdf: Company: " IntPol7072.CompanyCode 
    " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
    " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
    SKIP.
    OUTPUT CLOSE.
    RETURN. 
  END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE. */
  IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = (IntPol7072.PolicyTypeCd + "T") . /*Add by kridtiya i. date. 20160121*/
  FOR EACH FNameAttach WHERE
           FNameAttach.CompanyCode  = nv_NameCompCd
       AND FNameAttach.PolicyTypeCd = "V70"
       AND FNameAttach.CoverTypeCd  = nv_covcodtyp1    /*(IntPol7072.PolicyTypeCd + "T")*//*Add by kridtiya i. date. 20160121*/
       AND FNameAttach.EffDate     <= TODAY
  NO-LOCK
  BREAK BY FNameAttach.SelectNumber:
    IF FNameAttach.CopyFileName = "" THEN LEAVE.
                                     ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
    IF FNameAttach.ToFileName   = "" THEN
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
    ELSE DO:
      IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[BLANK]*/                      
           nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
                         + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
      ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
    END.
    nv_errortext = "".
    IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
    loop1:
    REPEAT:
      IF nv_firstchk = NO THEN DO:   
        NV_Lcount = 0.
        DO  WHILE NV_Lcount <= NV_Lwaitcount:
          NV_Lcount = NV_Lcount + 1.
        END.
      END.
      nv_firstchk = NO.
      IF SEARCH(nv_INPUTFileName) = ? THEN DO:
        NV_StartCount = NV_StartCount + NV_Lcount.
        IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
        NEXT loop1.
      END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                          nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
      DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
      IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
        DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
      END.
      IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
        FOR EACH TFileAttach: DELETE TFileAttach. END.
        CREATE TFileAttach.
        TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
        COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
        IF ERROR-STATUS:ERROR  THEN DO:
          nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
                       + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
        END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
        IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
        FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
        NO-ERROR NO-WAIT.
        IF AVAILABLE IntS7072 THEN DO:
          nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
          IF nv_LineSeqno = 1 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile1     = TFileAttach.FileBinary
            IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
            IntS7072.AttachFile1       = TFileAttach.FileBinary .
          END.
          IF nv_LineSeqno = 2 THEN DO:
            ASSIGN
            IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
            IntPol7072.AttachFile2     = TFileAttach.FileBinary
            IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
            IntS7072.AttachFile2       = TFileAttach.FileBinary .
          END.
          IF nv_errortext <> "" THEN
            ASSIGN
            IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
            IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
        END.
      END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
      LEAVE loop1.
    END. /*loop1:*/
  END. /*FOR EACH FNameAttach*/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt_Old C-Win 
PROCEDURE PD_SAVEPD1FileAtt_Old :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    ASSIGN
/*[COMMENT]*/                    NV_Lwaitcount = 110000
/*[COMMENT]*/                    NV_StartCount = 0
/*[COMMENT]*/                    NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
/*[COMMENT]*/                    FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[COMMENT]*/                    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[COMMENT]*/                    NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    nv_LineSeqno = 0.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                    PUT "1. Print :"
/*[COMMENT]*/                       /*12345678901*/
/*[COMMENT]*/                      IntPol7072.CompanyCode 
/*[COMMENT]*/                      IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                      IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                      TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                      STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                      SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    SKIP.
/*[COMMENT]*/                    OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/
/*[COMMENT]*/                       /* 2.1                             110 */
/*[COMMENT]*/                    IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
/*[COMMENT]*/                      RUN WSP/WSPMCpny.P
/*[COMMENT]*/                           (IntPol7072.CompanyCode
/*[COMMENT]*/                           ,IntPol7072.BranchCd
/*[COMMENT]*/                           ,"V70"
/*[COMMENT]*/                           ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[COMMENT]*/                           ,OUTPUT nv_BrokerBranch 
/*[COMMENT]*/                           ,OUTPUT nv_Acno1
/*[COMMENT]*/                           ,OUTPUT nv_Agent
/*[COMMENT]*/                           ,OUTPUT nv_errort).
/*[COMMENT]*/                      IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                      ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[COMMENT]*/                      RUN PD_FNameAttach
/*[COMMENT]*/                          (INPUT IntPol7072.CompanyCode
/*[COMMENT]*/                          ,INPUT "V70"      /*v70,v72*/
/*[COMMENT]*/                          ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                          /**/
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_NameCompCd
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_PrgName
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
/*[COMMENT]*/                      IF    IntPol7072.PolicyTypeCd = "1"
/*[COMMENT]*/                         OR IntPol7072.PolicyTypeCd = "2"
/*[COMMENT]*/                         OR IntPol7072.PolicyTypeCd = "3"
/*[COMMENT]*/                      THEN DO:
/*[COMMENT]*/                        /* Add by Kridtiya i...Lockton */
/*[COMMENT]*/                        IF IntPol7072.CompanyCode = "469" THEN DO:   
/*[COMMENT]*/                          IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr708".
/*[COMMENT]*/                          ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr708.p Form 1 Policy*/
/*[COMMENT]*/                          RUN VALUE(nv_PrgName)        
/*[COMMENT]*/                              (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[COMMENT]*/                              ,IntPol7072.PolicyNumber
/*[COMMENT]*/                              ,IntPol7072.Rencnt
/*[COMMENT]*/                              ,IntPol7072.Endcnt
/*[COMMENT]*/                              ,IntPol7072.DocumentUID
/*[COMMENT]*/                              ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[COMMENT]*/                              ,""               /*n_user   */
/*[COMMENT]*/                              ,""               /*n_passwd */
/*[COMMENT]*/                              ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                          RUN proc_FileAttach2               /* print cer */
/*[COMMENT]*/                              (INPUT IntPol7072.CompanyCode
/*[COMMENT]*/                              ,INPUT "V70"      /*v70,v72*/
/*[COMMENT]*/                              ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                              /**/
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_NameCompCd
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_PrgName
/*[COMMENT]*/                              ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                        END. /*End add Kridtiya i. */
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                          /*IF  IntPol7072.PolicyTypeCd = "3"    OR*/ /*kridtiya i.*/
/*[COMMENT]*/                           IF  (IntPol7072.PolicyTypeCd = "3"   AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
/*[COMMENT]*/                              /*TEST Prn cover 1*/
/*[COMMENT]*/                             (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )
/*[COMMENT]*/                              /*Isuzu */
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode  = "476" AND IntPol7072.PolicyTypeCd = "1" )
/*[COMMENT]*/                          THEN DO:
/*[COMMENT]*/                            IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
/*[COMMENT]*/                                               ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[COMMENT]*/                            RUN VALUE(nv_PrgName)
/*[COMMENT]*/                               (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
/*[COMMENT]*/                               ,IntPol7072.PolicyNumber
/*[COMMENT]*/                               ,IntPol7072.Rencnt
/*[COMMENT]*/                               ,IntPol7072.Endcnt
/*[COMMENT]*/                               ,IntPol7072.DocumentUID
/*[COMMENT]*/                               ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
/*[COMMENT]*/                               ,""               /*n_user  */
/*[COMMENT]*/                               ,""               /*n_passwd */
/*[COMMENT]*/                               ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ELSE DO:
/*[COMMENT]*/                        /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                        /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
/*[COMMENT]*/                        IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[COMMENT]*/                           OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" 
/*[COMMENT]*/                           OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"
/*[COMMENT]*/                           OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" ) /*isuzu*/
/*[COMMENT]*/                               /**/
/*[COMMENT]*/                           AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") )
/*[COMMENT]*/                                /*·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
/*[COMMENT]*/                        THEN DO:
/*[COMMENT]*/                          IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr703_1".
/*[COMMENT]*/                                             ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
/*[BLANK]*/                      
/*[COMMENT]*/                          RUN VALUE(nv_PrgName)
/*[COMMENT]*/                              (IntPol7072.CompanyCode
/*[COMMENT]*/                              ,IntPol7072.PolicyNumber
/*[COMMENT]*/                              ,IntPol7072.Rencnt
/*[COMMENT]*/                              ,IntPol7072.Endcnt
/*[COMMENT]*/                              ,IntPol7072.DocumentUID 
/*[COMMENT]*/                              ,IntPol7072.RqUID
/*[COMMENT]*/                              ,""
/*[COMMENT]*/                              ,""
/*[COMMENT]*/                              ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[COMMENT]*/                      NO-ERROR NO-WAIT.
/*[COMMENT]*/                      nv_firstchk   = YES.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "2. ChkPDF:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                      PAUSE 1 NO-MESSAGE.
/*[COMMENT]*/                      RUN PD_SAVEPD1ChkFile
/*[COMMENT]*/                          (INPUT nv_NameCompCd
/*[COMMENT]*/                          ,INPUT "V70"
/*[COMMENT]*/                          ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                          ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                      IF nv_SAVEmsgerror <> "" THEN DO:
/*[COMMENT]*/                        nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[COMMENT]*/                                  + ".TXT".
/*[BLANK]*/                      
/*[COMMENT]*/                        OUTPUT TO VALUE(nv_verror).
/*[COMMENT]*/                        PUT 
/*[COMMENT]*/                        "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[COMMENT]*/                        " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[COMMENT]*/                        " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[COMMENT]*/                        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                        SKIP.
/*[COMMENT]*/                        OUTPUT CLOSE.
/*[COMMENT]*/                        RETURN. 
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                      FOR EACH FNameAttach WHERE
/*[COMMENT]*/                               FNameAttach.CompanyCode  = nv_NameCompCd
/*[COMMENT]*/                           AND FNameAttach.PolicyTypeCd = "V70"
/*[COMMENT]*/                           AND FNameAttach.CoverTypeCd  = IntPol7072.PolicyTypeCd
/*[COMMENT]*/                           AND FNameAttach.EffDate     <= TODAY
/*[COMMENT]*/                      NO-LOCK
/*[COMMENT]*/                      BREAK BY FNameAttach.SelectNumber:
/*[COMMENT]*/                        IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[COMMENT]*/                                                         ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        IF FNameAttach.ToFileName   = "" THEN
/*[COMMENT]*/                               nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                          IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[BLANK]*/                      
/*[COMMENT]*/                               nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[COMMENT]*/                                             + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[COMMENT]*/                          ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        nv_errortext = "".
/*[COMMENT]*/                        IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        loop1:
/*[COMMENT]*/                        REPEAT:
/*[COMMENT]*/                          IF nv_firstchk = NO THEN DO:   
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          nv_firstchk = NO.
/*[COMMENT]*/                          IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[COMMENT]*/                            NV_StartCount = NV_StartCount + NV_Lcount.
/*[COMMENT]*/                            IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[COMMENT]*/                            NEXT loop1.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                          nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[COMMENT]*/                          DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[COMMENT]*/                          IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[COMMENT]*/                            DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[COMMENT]*/                            FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[COMMENT]*/                            CREATE TFileAttach.
/*[COMMENT]*/                            TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[COMMENT]*/                            COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
/*[COMMENT]*/                            IF ERROR-STATUS:ERROR  THEN DO:
/*[COMMENT]*/                              nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[COMMENT]*/                                           + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[COMMENT]*/                            END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[COMMENT]*/                            IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
/*[COMMENT]*/                            FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[COMMENT]*/                            NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE IntS7072 THEN DO:
/*[COMMENT]*/                              nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[COMMENT]*/                              IF nv_LineSeqno = 1 THEN DO:
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[COMMENT]*/                                IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[COMMENT]*/                              END.
/*[COMMENT]*/                              IF nv_LineSeqno = 2 THEN DO:
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[COMMENT]*/                                IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[COMMENT]*/                              END.
/*[COMMENT]*/                              IF nv_errortext <> "" THEN
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[COMMENT]*/                                IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[COMMENT]*/                          LEAVE loop1.
/*[COMMENT]*/                        END. /*loop1:*/
/*[COMMENT]*/                      END. /*FOR EACH FNameAttach*/
/*[COMMENT]*/                    END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /*  -------------------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
/*[COMMENT]*/                    IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[COMMENT]*/                      RUN WSP/WSPMCpny.P
/*[COMMENT]*/                           (IntPol7072.CompanyCode
/*[COMMENT]*/                           ,IntPol7072.BranchCd
/*[COMMENT]*/                           ,"V72"
/*[COMMENT]*/                           ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[COMMENT]*/                           ,OUTPUT nv_BrokerBranch 
/*[COMMENT]*/                           ,OUTPUT nv_Acno1
/*[COMMENT]*/                           ,OUTPUT nv_Agent
/*[COMMENT]*/                           ,OUTPUT nv_errort).
/*[COMMENT]*/                      IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      NV_StartCount = 0.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                      ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[BLANK]*/                      
/*[COMMENT]*/                      RUN PD_FNameAttach
/*[COMMENT]*/                          (INPUT        IntPol7072.CompanyCode
/*[COMMENT]*/                          ,INPUT        "V72"      /*v70,v72*/
/*[COMMENT]*/                          ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                          /**/
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_NameCompCd
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_PrgName
/*[COMMENT]*/                          ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
/*[COMMENT]*/                      /*Blank form Compulsory */
/*[COMMENT]*/                      IF IntPol7072.CompanyCode = "833" THEN DO:
/*[COMMENT]*/                        RUN PD_ChkBlankForm
/*[COMMENT]*/                             (INPUT-OUTPUT nv_NameCompCd
/*[COMMENT]*/                             ,INPUT-OUTPUT nv_PrgName
/*[COMMENT]*/                             ,INPUT-OUTPUT nv_PrmPrg).
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      IF nv_PrgName = "" THEN DO:
/*[COMMENT]*/                        RUN Wctx/wctxr702.P
/*[COMMENT]*/                             (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[COMMENT]*/                             ,IntPol7072.CMIPolicyNumber
/*[COMMENT]*/                             ,IntPol7072.Rencnt
/*[COMMENT]*/                             ,IntPol7072.Endcnt
/*[COMMENT]*/                             ,IntPol7072.CMIDocumentUID
/*[COMMENT]*/                             ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[COMMENT]*/                             ,""         /*n_user   */
/*[COMMENT]*/                             ,""         /*n_passwd */
/*[COMMENT]*/                             ,nv_PrmPrg  /*Name Report*/
/*[COMMENT]*/                             ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ELSE DO:
/*[COMMENT]*/                        nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*
/*[COMMENT]*/                        RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[COMMENT]*/                        RUN VALUE(nv_PrgName)
/*[COMMENT]*/                             (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[COMMENT]*/                             ,IntPol7072.CMIPolicyNumber
/*[COMMENT]*/                             ,IntPol7072.Rencnt
/*[COMMENT]*/                             ,IntPol7072.Endcnt
/*[COMMENT]*/                             ,IntPol7072.CMIDocumentUID
/*[COMMENT]*/                             ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[COMMENT]*/                             ,""        /*n_user   */
/*[COMMENT]*/                             ,""        /*n_passwd */
/*[COMMENT]*/                             ,nv_PrmPrg /*Name Report="V72A4"*/
/*[COMMENT]*/                             ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "2. ChkPDF:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                      PAUSE 1 NO-MESSAGE.
/*[COMMENT]*/                      RUN PD_SAVEPD1ChkFile
/*[COMMENT]*/                          (INPUT nv_NameCompCd
/*[COMMENT]*/                          ,INPUT "V72"
/*[COMMENT]*/                          ,INPUT IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                          ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                      IF nv_SAVEmsgerror <> "" THEN DO:
/*[COMMENT]*/                        nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[COMMENT]*/                                  + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[COMMENT]*/                                  + ".TXT".
/*[COMMENT]*/                        OUTPUT TO VALUE(nv_verror).
/*[COMMENT]*/                        PUT 
/*[COMMENT]*/                        "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[COMMENT]*/                        " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[COMMENT]*/                        " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[COMMENT]*/                        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                        SKIP.
/*[COMMENT]*/                        OUTPUT CLOSE.
/*[COMMENT]*/                        RETURN. 
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO PrnTIME.txt APPEND.
/*[COMMENT]*/                      PUT "3. SvFile:"
/*[COMMENT]*/                         /*1234567890*/
/*[COMMENT]*/                        IntPol7072.CompanyCode 
/*[COMMENT]*/                        IntPol7072.PolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[COMMENT]*/                        TODAY FORMAT "99/99/9999" " "
/*[COMMENT]*/                        STRING(TIME,"HH:MM:SS")   "." 
/*[COMMENT]*/                        SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    */
/*[COMMENT]*/                      ASSIGN
/*[COMMENT]*/                      nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[COMMENT]*/                      FOR EACH FNameAttach WHERE
/*[COMMENT]*/                               FNameAttach.CompanyCode  = nv_NameCompCd
/*[COMMENT]*/                           AND FNameAttach.PolicyTypeCd = "V72"
/*[COMMENT]*/                           AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
/*[COMMENT]*/                           AND FNameAttach.EffDate     <= TODAY
/*[COMMENT]*/                      NO-LOCK
/*[COMMENT]*/                      BREAK BY FNameAttach.SelectNumber
/*[COMMENT]*/                      :
/*[COMMENT]*/                        IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[COMMENT]*/                                                         ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        IF FNameAttach.ToFileName   = "" THEN
/*[COMMENT]*/                               nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                          IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[COMMENT]*/                               nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[COMMENT]*/                                             + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[COMMENT]*/                          ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        nv_errortext = "".
/*[COMMENT]*/                        IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
/*[COMMENT]*/                                nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        loop2:
/*[COMMENT]*/                        REPEAT:
/*[COMMENT]*/                          IF nv_firstchk = NO THEN DO:   
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          nv_firstchk = NO.
/*[COMMENT]*/                          IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[COMMENT]*/                            NV_StartCount = NV_StartCount + NV_Lcount.
/*[COMMENT]*/                            IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
/*[COMMENT]*/                            NEXT loop2.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[COMMENT]*/                          IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[COMMENT]*/                            FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[COMMENT]*/                            CREATE TFileAttach.
/*[COMMENT]*/                            TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            IF ERROR-STATUS:ERROR  THEN DO:
/*[COMMENT]*/                              nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[COMMENT]*/                                           + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[COMMENT]*/                            END.
/*[COMMENT]*/                            IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
/*[COMMENT]*/                            FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[COMMENT]*/                            NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE IntS7072 THEN DO:
/*[COMMENT]*/                              nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
/*[COMMENT]*/                              IF nv_LineSeqno = 1 THEN DO:
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[COMMENT]*/                                IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[COMMENT]*/                              END.
/*[COMMENT]*/                              IF nv_LineSeqno = 2 THEN DO:
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[COMMENT]*/                                IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[COMMENT]*/                                IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[COMMENT]*/                              END.
/*[COMMENT]*/                              IF nv_errortext <> "" THEN
/*[COMMENT]*/                                ASSIGN
/*[COMMENT]*/                                IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[COMMENT]*/                                IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          LEAVE loop2.
/*[COMMENT]*/                        END. /*loop2:*/
/*[COMMENT]*/                      END. /*FOR EACH FNameAttach*/
/*[COMMENT]*/                    END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[COMMENT]*/                    FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[COMMENT]*/                    RELEASE FNameAttach.  */
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1F_PrgName C-Win 
PROCEDURE PD_SAVEPD1F_PrgName :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER nv_RECIDIntPol7072  AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_PrgName          AS CHARACTER NO-UNDO.
ASSIGN nv_covcodtyp1 = "".
/*[BLANK]*/                      
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
IF AVAIL IntPol7072 THEN DO:
/*[COMMENT]*/                        /* Line : 70 + 72 compulsaly */
    IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:  
        IF IntPol7072.PolicyTypeCd = "1" THEN DO: 
            ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "1T". 
/*[COMMENT]*/                                /*comment by kridtiya i. »Ô´ Form Knock 
/*[COMMENT]*/                                IF (IntPol7072.RateGroup = "220") OR (IntPol7072.RateGroup = "230") THEN DO:
/*[COMMENT]*/                                     IF (inte(IntPol7072.SeatingCapacity) > 15)          THEN ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "1T". 
/*[COMMENT]*/                                                                                         ELSE ASSIGN nv_PrgName = "wctxis76A4"   nv_covcodtyp1 = "1KT". 
/*[COMMENT]*/                                END.
/*[COMMENT]*/                                ELSE IF (IntPol7072.RateGroup = "320") OR (IntPol7072.RateGroup = "420") OR (IntPol7072.RateGroup = "520") THEN DO:
/*[COMMENT]*/                                     IF (inte(IntPol7072.GrossVehOrCombinedWeight) > 3 ) THEN ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "1T". 
/*[COMMENT]*/                                                                                         ELSE ASSIGN nv_PrgName = "wctxis76A4"   nv_covcodtyp1 = "1KT".
/*[COMMENT]*/                                END.
/*[COMMENT]*/                                ELSE ASSIGN nv_PrgName = "wctxis76A4"   nv_covcodtyp1 = "1KT".
/*[COMMENT]*/                                end...comment by kridtiya i. »Ô´ Form Knock */
        END.                                                         
        ELSE IF IntPol7072.PolicyTypeCd = "3"                        THEN ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "3T". 
        ELSE IF IntPol7072.PolicyTypeCd = "2.1"                      THEN ASSIGN nv_PrgName = "wctxis74A4"   nv_covcodtyp1 = "2.1T". 
        ELSE IF IntPol7072.PolicyTypeCd = "3.1"                      THEN ASSIGN nv_PrgName = "wctxis74A4"   nv_covcodtyp1 = "3.1T". 
        ELSE IF IntPol7072.PolicyTypeCd = "2.2"                      THEN ASSIGN nv_PrgName = "wctxis78A4"   nv_covcodtyp1 = "2.2T". 
        ELSE IF IntPol7072.PolicyTypeCd = "3.2"                      THEN ASSIGN nv_PrgName = "wctxis78A4"   nv_covcodtyp1 = "3.2T". 
/*[BLANK]*/                      
/*[COMMENT]*/                            /*4U ». 2+ 3+   ÍÂèÒ§à´ÕÂÇ  ** cover 2.1  3.1   wctxizu707.p  SCHEDULE8 * +++ *  */
    END.
    ELSE DO:
/*[COMMENT]*/                            /* Line : 70  only */
        IF IntPol7072.PolicyTypeCd = "1" THEN DO: 
            ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "1". 
/*[COMMENT]*/                                /*comment by kridtiya i. »Ô´ Form Knock .....
/*[COMMENT]*/                                IF (IntPol7072.RateGroup = "220") OR (IntPol7072.RateGroup = "230") THEN DO:
/*[COMMENT]*/                                     IF (inte(IntPol7072.SeatingCapacity) > 15)          THEN ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "1". 
/*[COMMENT]*/                                                                                         ELSE ASSIGN nv_PrgName = "wctxis75A4"   nv_covcodtyp1 = "1K". 
/*[COMMENT]*/                                END.
/*[COMMENT]*/                                ELSE IF (IntPol7072.RateGroup = "320") OR (IntPol7072.RateGroup = "420") OR (IntPol7072.RateGroup = "520") THEN DO:
/*[COMMENT]*/                                     IF (inte(IntPol7072.GrossVehOrCombinedWeight) > 3 ) THEN ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "1". 
/*[COMMENT]*/                                                                                         ELSE ASSIGN nv_PrgName = "wctxis75A4"   nv_covcodtyp1 = "1K".
/*[COMMENT]*/                                END.
/*[COMMENT]*/                                ELSE ASSIGN nv_PrgName = "wctxis75A4"   nv_covcodtyp1 = "1K".
/*[COMMENT]*/                                end...comment by kridtiya i. »Ô´ Form Knock */
/*[BLANK]*/                      
        END.      
        ELSE IF IntPol7072.PolicyTypeCd = "3"                        THEN ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "3". 
        ELSE IF IntPol7072.PolicyTypeCd = "2.1"                      THEN ASSIGN nv_PrgName = "wctxis73A4"   nv_covcodtyp1 = "2.1".
        ELSE IF IntPol7072.PolicyTypeCd = "3.1"                      THEN ASSIGN nv_PrgName = "wctxis73A4"   nv_covcodtyp1 = "3.1".
        ELSE IF IntPol7072.PolicyTypeCd = "2.2"                      THEN ASSIGN nv_PrgName = "wctxis77A4"   nv_covcodtyp1 = "2.2".
        ELSE IF IntPol7072.PolicyTypeCd = "3.2"                      THEN ASSIGN nv_PrgName = "wctxis77A4"   nv_covcodtyp1 = "3.2". 
/*[COMMENT]*/                            /*4U ». 2+ 3+   ÍÂèÒ§à´ÕÂÇ  ** cover 2.1  3.1   wctxizu707.p  SCHEDULE7 * +++ *  */    
    END.
END.
/*[BLANK]*/                      
/*[BLANK]*/                      
OUTPUT TO PD_SAVEPD1F_PrgName.TXT APPEND.
PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
  nv_covcodtyp1 FORMAT "x(5)"
/*[COMMENT]*/                      /*
  nv_NameCompCd FORMAT "x(20)" /*210*/ */
  nv_PrgName    FORMAT "x(20)" /*Wctxr701A4*/
/*[COMMENT]*/                      /*
  nv_PrmPrg     FORMAT "x(20)" /*V70A4*/ */
SKIP.
OUTPUT CLOSE.
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1IntS C-Win 
PROCEDURE PD_SAVEPD1IntS :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       à¡çº¢éÍÁÙÅ ·Ø¹»ÃÐ¡Ñ¹ àºÕéÂÊØ·¸Ô ÍÒ¡Ã áÊµÁ»ì ÊÓËÃÑºÊè§µÍº¡ÅÑº
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
DEFINE INPUT PARAMETER nv_rec_rq          AS RECID NO-UNDO.
DEFINE INPUT PARAMETER nv_RecIntPol7072   AS RECID NO-UNDO.
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[BLANK]*/                      
FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[BLANK]*/                      
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
NO-LOCK NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÀÒ¤ÊÁÑ¤Ãã¨ */
ASSIGN
/*[COMMENT]*/                      /*
  = IntPol7072.DRIVER_TITLE_1    /*uwm100.policy    /Policy no.*/ */
/*[BLANK]*/                      
  IntS7072.COLLAmtAccident = IntPol7072.DRIVER_NAME_1      /*nv_SUM_INSURE    /COLLAmtAccident*/
  IntS7072.FTAmt           = IntPol7072.DRIVER_SURNAME_1   /*nv_SUM_INSUREAcc /FTAmt*/
  IntS7072.WrittenAmt      = IntPol7072.DRIVER_GENDER_1    /*uwm100.prem_t    /WrittenAmt*/ 
  IntS7072.RevenueStampAmt = IntPol7072.DRIVER_AGE_1       /*uwm100.rstp_t    /RevenueStampAmt*/
  IntS7072.VatAmt          = IntPol7072.DRIVER_LICENSE_1.  /*uwm100.rtax_t    /VatAmt*/
/*[BLANK]*/                      
IntS7072.CurrentTermAmt  = STRING( DECIMAL(IntS7072.WrittenAmt)
                                 + DECIMAL(IntS7072.RevenueStampAmt)
                                 + DECIMAL(IntS7072.VatAmt), ">>>,>>>,>>9.99") NO-ERROR.
/*[COMMENT]*/                    /* ------------------- */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÀÒ¤ºÑ§¤Ñº (¾.Ã.º.) */
ASSIGN
/*[COMMENT]*/                      /*
  = IntPol7072.DRIVER_TITLE_2    /*uwm100.policy    /Policy no.*/ */
  IntS7072.CMIAmtPerson       = IntPol7072.DRIVER_NMAE_2     /*nv_SUM_INSURE    /COLLAmtAccident*/
  IntS7072.CMIAmtAccident     = IntPol7072.DRIVER_SURNAME_2  /*nv_SUM_INSUREAcc /FTAmt*/
  IntS7072.CMIWrittenAmt      = IntPol7072.DRIVER_GENDER_2   /*uwm100.prem_t    /WrittenAmt*/
  IntS7072.CMIRevenueStampAmt = IntPol7072.DRIVER_AGE_2      /*uwm100.rstp_t    /RevenueStampAmt*/
  IntS7072.CMIVatAmt          = IntPol7072.DRIVER_LICENSE_2. /*uwm100.rtax_t    /VatAmt*/
/*[BLANK]*/                      
IntS7072.CMICurrentTermAmt  = STRING( DECIMAL(IntS7072.CMIWrittenAmt)
                                    + DECIMAL(IntS7072.CMIRevenueStampAmt)
                                    + DECIMAL(IntS7072.CMIVatAmt), ">>>,>>>,>>9.99") NO-ERROR.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------- */
OUTPUT STREAM xmlstream TO PUT_PD_SAVEPD1IntS.TXT.
PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
    " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
PUT STREAM xmlstream "27 WrittenAmt      |" IntPol7072.DRIVER_NMAE_2      SKIP.
PUT STREAM xmlstream "5  CMIPolicyNumber |" IntPol7072.DRIVER_SURNAME_2   FORMAT "x(20)" SKIP.
PUT STREAM xmlstream "7  CMIDocumentUID  |" IntPol7072.DRIVER_GENDER_2    FORMAT "X(20)" SKIP.
PUT STREAM xmlstream "7  CMIBarCodeNumber|" IntPol7072.DRIVER_AGE_2       FORMAT "X(20)" SKIP.
PUT STREAM xmlstream "32 CMIPolicyTypeCd |" IntPol7072.DRIVER_LICENSE_2   SKIP. 
PUT STREAM xmlstream  FILL("-",78) FORMAT "X(78)" SKIP(1).
OUTPUT STREAM xmlstream CLOSE.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_01 C-Win 
PROCEDURE PD_SAVEPD1_01 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER nv_okprn   AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER nv_trty11  AS CHARACTER NO-UNDO.   /*M,T*/
DEFINE INPUT PARAMETER nv_docno1  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nv_STdocno AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nv_STKNo   AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.*/
/*[COMMENT]*/                    /*DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.*/
/*[BLANK]*/                      
IF  /* SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3" OR                IntPol7072.CMIPolicyTypeCd    <> ""*/
/*[COMMENT]*/                     /*  OR ((IntPol7072.CompanyCode = "833") OR (IntPol7072.CompanyCode = "570" ) OR (IntPol7072.CompanyCode = "839") OR (IntPol7072.CompanyCode = "1098")
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "210")  OR (IntPol7072.CompanyCode = "M82")  OR (IntPol7072.CompanyCode = "444") OR (IntPol7072.CompanyCode = "1103") 
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "1141") OR (IntPol7072.CompanyCode = "1107") OR (IntPol7072.CompanyCode = "1056") OR (IntPol7072.CompanyCode = "M73") 
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "1012") OR (IntPol7072.CompanyCode = "M85")  OR (IntPol7072.CompanyCode = "1146") OR (IntPol7072.CompanyCode = "1018")
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "834")  OR (IntPol7072.CompanyCode = "1470") OR (IntPol7072.CompanyCode = "1554") OR (IntPol7072.CompanyCode = "1752")
/*[COMMENT]*/                       AND */
       (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q"  /* AND nv_okprn = YES*/   )
/*[COMMENT]*/                    /**AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2")
/*[COMMENT]*/                       AND DECIMAL(IntPol7072.SumInsureAmt) <= 200000) */
THEN DO:
  IF     IntPol7072.DocumentUID      = "" /*nv_DocnoV70*/
     AND IntPol7072.CMIDocumentUID   = "" /*nv_DocnoV72*/
/*[COMMENT]*/                       /*  AND IntS7072.StickerNumber    = ""
/*[COMMENT]*/                         AND IntS7072.CMIBarCodeNumber = "" */
  THEN DO:  
    ASSIGN nv_trty11 = "" nv_docno1 = "" nv_STdocno = "".
    IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3" OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "2" /*
/*[COMMENT]*/                            ((IntPol7072.CompanyCode = "833")  OR (IntPol7072.CompanyCode = "834")  OR (IntPol7072.CompanyCode = "570")  OR (IntPol7072.CompanyCode = "210")
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1098") OR (IntPol7072.CompanyCode = "M85")  OR (IntPol7072.CompanyCode = "M82")  OR (IntPol7072.CompanyCode = "M73") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1103") OR (IntPol7072.CompanyCode = "1107") OR (IntPol7072.CompanyCode = "1141") OR (IntPol7072.CompanyCode = "1146") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1018") OR (IntPol7072.CompanyCode = "444")  OR (IntPol7072.CompanyCode = "1056") OR (IntPol7072.CompanyCode = "1012") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "839")  OR (IntPol7072.CompanyCode = "1470") OR (IntPol7072.CompanyCode = "1554") OR (IntPol7072.CompanyCode = "1752"))
/*[COMMENT]*/                          AND ( IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2" OR  IntPol7072.PolicyTypeCd = "2.3" 
         OR IntPol7072.PolicyTypeCd = "2.4" OR IntPol7072.PolicyTypeCd = "2.5" OR IntPol7072.PolicyTypeCd = "2.6" )*/ THEN DO:
/*[COMMENT]*/                          /*FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                               FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[COMMENT]*/                           AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                          IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          ELSE DO:
/*[COMMENT]*/                            FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                                 FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[COMMENT]*/                             AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                            NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          END.*/ 
        nv_STdocno = "YES".
        IF nv_STdocno = "YES" THEN DO:
            nv_trty11       = "M".
            nv_msgerror7072 = "".
/*[COMMENT]*/                                /*RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072).*/
/*[COMMENT]*/                                /*RUN WXZ\WXZCDOC0.p (INPUT  1    /*nv_choice */*/
            RUN WXZ\WXZCDOCWOS.p (INPUT  1    /*nv_choice */
                           ,INPUT  "M"       /*nv_DocType*/
                           ,INPUT  IntPol7072.CompanyCode    
                           ,INPUT  IntPol7072.Username      
                           ,INPUT  NO            
                           ,OUTPUT nv_docno1
                           ,OUTPUT nv_docrun2
                           ,OUTPUT nv_docrun3
                           ,OUTPUT nv_msgerror7072).
            ASSIGN
                IntPol7072.DocumentUID = nv_docno1
                IntS7072.DocumentUID   = nv_docno1.
        END.
        OUTPUT TO PD_SAVEPD_01pin_BLKDOC.TXT APPEND.
        PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
            " PD_SAVEPD1_01: 70. Start: " FORMAT "x(25)"   SKIP.
        PUT "company : " IntPol7072.CompanyCode FORMAT "x(5)"   
            " nv_STdocno: " nv_STdocno FORMAT "x(5)"  
          " nv_docno1: "  nv_docno1  FORMAT "9999999"
           " nv_STKNo:  "  nv_STKNo  FORMAT "x(15)"  
        SKIP.
        OUTPUT CLOSE.
    END.
    IF nv_msgerror7072 <> "" THEN DO:
        ASSIGN
            IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
      IntPol7072.GenSicBran     = NO
      IntPol7072.GenSicBranText = nv_msgerror7072
      IntPol7072.GenSicBranST   = "ERROR"
      IntPol7072.ErrorMessage   = nv_msgerror7072.
      IF IntPol7072.DocumentUID <> "" THEN
         IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
      OUTPUT TO PD_SAVEPD_01pin_BLKDOC.TXT APPEND.
      PUT "70 : ERROR" SKIP.
      OUTPUT CLOSE.
      RETURN.
    END.
/*[COMMENT]*/                        /**/
    IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                           /* 
/*[COMMENT]*/                          FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                               FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[COMMENT]*/                           AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                          IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          ELSE DO:
/*[COMMENT]*/                            FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                                 FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[COMMENT]*/                             AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                            NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          END.  */
      nv_STdocno = "YES".
      IF nv_STdocno = "YES" THEN DO:
        nv_trty11       = "T".
        nv_msgerror7072 = "".
/*[COMMENT]*/                            /*RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072).*/
/*[COMMENT]*/                            /*RUN WXZ\WXZCDOC0.p (INPUT  1        /*nv_choice */*/
        RUN WXZ\WXZCDOCWOS.p (INPUT  1        /*nv_choice */
                            ,INPUT  "M"       /*nv_DocType*/
                            ,INPUT  IntPol7072.CompanyCode  
                            ,INPUT  IntPol7072.Username      
                            ,INPUT  NO  
                            ,OUTPUT nv_docno1
                            ,OUTPUT nv_docrun2
                            ,OUTPUT nv_docrun3
                            ,OUTPUT nv_msgerror7072).
        IF nv_msgerror7072 = "" THEN
/*[COMMENT]*/                                /*RUN WXZ\WXZCDOC0.p (INPUT  1        /*nv_choice */*/
            RUN WXZ\WXZCDOCWOS.p (INPUT  1        /*nv_choice */
                                ,INPUT  "T"       /*nv_DocType*/
                                ,INPUT  IntPol7072.CompanyCode 
                                ,INPUT  IntPol7072.Username      
                                ,INPUT  NO  
                                ,OUTPUT nv_STKNo
                                ,OUTPUT nv_docrun2
                                ,OUTPUT nv_docrun3
                                ,OUTPUT nv_msgerror7072).
/*[BLANK]*/                                  
        ASSIGN
        IntPol7072.CMIBarCodeNumber = nv_STKNo
        IntS7072.CMIBarCodeNumber   = nv_STKNo
        IntPol7072.CMIDocumentUID   = nv_docno1
        IntS7072.CMIDocumentUID     = nv_docno1 .
/*[BLANK]*/                      
        OUTPUT TO PD_SAVEPD_01pin_BLKDOC72.TXT APPEND.
        PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
            " PD_SAVEPD1_01: 72. Start: " FORMAT "x(25)"   SKIP.
        PUT "company : " IntPol7072.CompanyCode FORMAT "x(5)"   
            " nv_STdocno: " nv_STdocno FORMAT "x(15)"  
            " nv_docno1: "  nv_docno1  FORMAT "x(15)" 
            " nv_STKNo:  "  nv_STKNo   FORMAT "x(15)"  
        SKIP.
        OUTPUT CLOSE.
      END.
    END.
/*[BLANK]*/                      
    IF nv_msgerror7072 <> "" THEN DO:
      ASSIGN
      IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
      IntPol7072.GenSicBran     = NO
      IntPol7072.GenSicBranText = nv_msgerror7072
      IntPol7072.GenSicBranST   = "ERROR"
      IntPol7072.ErrorMessage   = nv_msgerror7072.
      IF IntPol7072.DocumentUID <> "" THEN
         IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[BLANK]*/                      
        OUTPUT TO PD_SAVEPD_01pin_BLKDOC_ERR.TXT APPEND.
        PUT "72 : ERROR"  IntPol7072.CompanyCode SKIP.
        OUTPUT CLOSE.
      RETURN.
    END.
  END.
END.
/*[COMMENT]*/                    /**/
IF SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q" THEN DO:
  IF    (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND IntPol7072.DocumentUID = "")    /*nv_DocnoV70*/
     OR (               IntPol7072.CMIPolicyTypeCd   <> ""  AND IntPol7072.CMIDocumentUID = "") /*nv_DocnoV72*/ 
/*[COMMENT]*/                         /**/
     OR ( ( (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
     AND  nv_okprn = YES ) AND IntPol7072.DocumentUID = "")
  THEN DO:
    ASSIGN
    IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
    IntPol7072.GenSicBran     = NO
    IntPol7072.GenSicBranText = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº"
    IntPol7072.GenSicBranST   = "ERROR"
    IntPol7072.ErrorMessage   = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº".
    IF IntPol7072.DocumentUID <> "" THEN
       IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
    RETURN.
  END.
END. 
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_02 C-Win 
PROCEDURE PD_SAVEPD1_02 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       70+72 
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /* CASE: 72  2.1 2.2 3.1 3.2 */
IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
    nv_msgerror7072 = "".
    RUN UZ/UZO7201WS.P   /* create 72 */
        (nv_RecIntPol7072
         ,INPUT-OUTPUT nv_msgerror7072).
    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
      NO-ERROR.
    IF nv_msgerror7072 <> "" THEN DO:
        OUTPUT TO WSPGP100-ERROR.TXT APPEND.
        PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
            TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
            " " nv_msgerror7072 FORMAT "X(150)" SKIP.
        OUTPUT CLOSE.
    END.
END.  /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
/*[COMMENT]*/                    /* CREATE : 70 */
/*[COMMENT]*/                    /*IF IntPol7072.PolicyTypeCd = "1"  THEN DO:*/
IF IntPol7072.PolicyTypeCd <> ""  THEN DO:    /*kridtiya i. */
    RUN WRS/WRSGU100.P
        (nv_RecIntPol7072
         ,INPUT-OUTPUT nv_msgerror7072).
    IF nv_msgerror7072 <> "" THEN DO:
        OUTPUT TO WRSGU100-ERROR.TXT APPEND.
        PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
            TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
            " " nv_msgerror7072 FORMAT "X(150)" SKIP.
        OUTPUT CLOSE.
    END.
END.
IF nv_msgerror7072 <> "" THEN DO:
    OUTPUT TO WRSGU100-ERROR.TXT APPEND.
    PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
        TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
        " " nv_msgerror7072 FORMAT "X(150)" SKIP.
    OUTPUT CLOSE.
    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
        NO-ERROR.
    IF AVAILABLE IntPol7072 THEN DO:
        ASSIGN
            IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
            IntPol7072.GenSicBran   = NO
            IntPol7072.GenSicBranText = nv_msgerror7072
            IntPol7072.GenSicBranST = "ERROR"
            IntPol7072.ErrorMessage = nv_msgerror7072.
        IF IntPol7072.DocumentUID <> "" THEN
            IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
    END.
    RETURN.
END.
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_OLD C-Win 
PROCEDURE PD_SAVEPD1_OLD :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE INPUT-OUTPUT PARAMETER  nv_recinout7072   AS RECID     NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_trty11  AS CHARACTER NO-UNDO. /*M,T*/
/*[COMMENT]*/                    DEFINE VARIABLE nv_docno1  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_STdocno AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_STKNo   AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_okprn AS LOGICAL   NO-UNDO.
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[COMMENT]*/                    RUN PD_ConDBExp. /* DB EXPIRY */
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[COMMENT]*/                    nv_resulttext = "".
/*[COMMENT]*/                    FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[COMMENT]*/                    NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[COMMENT]*/                    /*¡ÃÁ¸ÃÃÁì»... + ¾Ãº. */
/*[COMMENT]*/                    IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN DO:
/*[COMMENT]*/                      nv_octets = "".
/*[COMMENT]*/                      FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70 /*70,72*/
/*[COMMENT]*/                      NO-ERROR NO-WAIT.
/*[COMMENT]*/                      IF NOT AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                        RUN WRS/WRSDigit.p (output nv_octets).
/*[COMMENT]*/                        CREATE IntPol7072.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ELSE nv_octets = IntPol7072.RqUID.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    ELSE DO:   /* ¡ÃÁ¸ÃÃÁì ¾Ãº.*/
/*[COMMENT]*/                      IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
/*[COMMENT]*/                        FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72
/*[COMMENT]*/                        NO-ERROR NO-WAIT.
/*[COMMENT]*/                        IF NOT AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                          nv_octets = "".
/*[COMMENT]*/                          RUN WRS/WRSDigit.p (output nv_octets).
/*[COMMENT]*/                          CREATE IntPol7072.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE nv_octets = IntPol7072.RqUID.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    nv_RecIntPol7072 = RECID(IntPol7072).
/*[COMMENT]*/                    nv_recinout7072  = RECID(IntPol7072).
/*[COMMENT]*/                    RUN PD_SAVEPD2.    /*Save data to IntPol7072*/
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[COMMENT]*/                    /* ËÒàÅ¢ Docno1 */
/*[COMMENT]*/                    FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[COMMENT]*/                    NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[COMMENT]*/                    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                    NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    /*20/3/2015*/
/*[COMMENT]*/                    nv_okprn = NO.
/*[COMMENT]*/                    RUN WRS\WRSFutil.p 
/*[COMMENT]*/                        (IntPol7072.PolicyTypeCd
/*[COMMENT]*/                        ,IntPol7072.SumInsureAmt
/*[COMMENT]*/                        ,IntPol7072.CompanyCode
/*[COMMENT]*/                        ,INPUT-OUTPUT nv_okprn).
/*[COMMENT]*/                    /* ---------------------------------------------------- */
/*[COMMENT]*/                    IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3"
/*[COMMENT]*/                       OR                IntPol7072.CMIPolicyTypeCd    <> ""
/*[COMMENT]*/                       OR  (IntPol7072.CompanyCode = "833" AND 
/*[COMMENT]*/                           (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[COMMENT]*/                       AND nv_okprn = YES )
/*[COMMENT]*/                    /**AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2")
/*[COMMENT]*/                       AND DECIMAL(IntPol7072.SumInsureAmt) <= 200000) */
/*[COMMENT]*/                    THEN DO:
/*[COMMENT]*/                      IF     IntS7072.DocumentUID      = "" /*nv_DocnoV70*/
/*[COMMENT]*/                         AND IntS7072.CMIDocumentUID   = "" /*nv_DocnoV72*/
/*[COMMENT]*/                         AND IntS7072.StickerNumber    = ""
/*[COMMENT]*/                         AND IntS7072.CMIBarCodeNumber = ""
/*[COMMENT]*/                      THEN DO:
/*[COMMENT]*/                        ASSIGN nv_trty11 = "" nv_docno1 = "" nv_STdocno = "".
/*[COMMENT]*/                        IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3"
/*[COMMENT]*/                           OR (IntPol7072.CompanyCode = "833" AND 
/*[COMMENT]*/                         (     IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2"
/*[COMMENT]*/                           OR  IntPol7072.PolicyTypeCd = "2.3" OR IntPol7072.PolicyTypeCd = "2.4"
/*[COMMENT]*/                           OR  IntPol7072.PolicyTypeCd = "2.5" OR IntPol7072.PolicyTypeCd = "2.6" ) )
/*[COMMENT]*/                        THEN DO:
/*[COMMENT]*/                          FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                               FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[COMMENT]*/                           AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                          IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          ELSE DO:
/*[COMMENT]*/                            FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                                 FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[COMMENT]*/                             AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                            NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          IF nv_STdocno = "YES" THEN DO:
/*[COMMENT]*/                            nv_trty11       = "M".
/*[COMMENT]*/                            nv_msgerror7072 = "".
/*[COMMENT]*/                            RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072
/*[COMMENT]*/                                ).
/*[COMMENT]*/                            ASSIGN
/*[COMMENT]*/                            IntPol7072.DocumentUID = nv_docno1
/*[COMMENT]*/                            IntS7072.DocumentUID   = nv_docno1.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                          ASSIGN
/*[COMMENT]*/                          IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[COMMENT]*/                          IntPol7072.GenSicBran     = NO
/*[COMMENT]*/                          IntPol7072.GenSicBranText = nv_msgerror7072
/*[COMMENT]*/                          IntPol7072.GenSicBranST   = "ERROR"
/*[COMMENT]*/                          IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[COMMENT]*/                          IF IntPol7072.DocumentUID <> "" THEN
/*[COMMENT]*/                             IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[COMMENT]*/                          RETURN.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                          FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                               FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                           AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[COMMENT]*/                           AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                          NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                          IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          ELSE DO:
/*[COMMENT]*/                            FIND FIRST FUtilSetUp WHERE
/*[COMMENT]*/                                 FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[COMMENT]*/                             AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[COMMENT]*/                             AND FUtilSetUp.EffDate    <= TODAY
/*[COMMENT]*/                            NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                            IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          IF nv_STdocno = "YES" THEN DO:
/*[COMMENT]*/                            nv_trty11       = "T".
/*[COMMENT]*/                            nv_msgerror7072 = "".
/*[COMMENT]*/                            RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072
/*[COMMENT]*/                                ).
/*[COMMENT]*/                            ASSIGN
/*[COMMENT]*/                            IntPol7072.CMIBarCodeNumber = nv_STKNo
/*[COMMENT]*/                            IntS7072.CMIBarCodeNumber   = nv_STKNo
/*[COMMENT]*/                            IntPol7072.CMIDocumentUID = nv_docno1
/*[COMMENT]*/                            IntS7072.CMIDocumentUID   = nv_docno1 .
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                          ASSIGN
/*[COMMENT]*/                          IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[COMMENT]*/                          IntPol7072.GenSicBran     = NO
/*[COMMENT]*/                          IntPol7072.GenSicBranText = nv_msgerror7072
/*[COMMENT]*/                          IntPol7072.GenSicBranST   = "ERROR"
/*[COMMENT]*/                          IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[COMMENT]*/                          IF IntPol7072.DocumentUID <> "" THEN
/*[COMMENT]*/                             IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[COMMENT]*/                          RETURN.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    IF SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q" THEN DO:
/*[COMMENT]*/                      IF    (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND IntPol7072.DocumentUID = "") /*nv_DocnoV70*/
/*[COMMENT]*/                         OR (               IntPol7072.CMIPolicyTypeCd   <> ""  AND IntPol7072.CMIDocumentUID = "") /*nv_DocnoV72*/ 
/*[COMMENT]*/                         /**/
/*[COMMENT]*/                         OR ( ( (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[COMMENT]*/                         AND  nv_okprn = YES ) AND IntPol7072.DocumentUID = "")
/*[COMMENT]*/                      THEN DO:
/*[COMMENT]*/                        ASSIGN
/*[COMMENT]*/                        IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[COMMENT]*/                        IntPol7072.GenSicBran     = NO
/*[COMMENT]*/                        IntPol7072.GenSicBranText = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº"
/*[COMMENT]*/                        IntPol7072.GenSicBranST   = "ERROR"
/*[COMMENT]*/                        IntPol7072.ErrorMessage   = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº".
/*[COMMENT]*/                        IF IntPol7072.DocumentUID <> "" THEN
/*[COMMENT]*/                           IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[COMMENT]*/                        RETURN.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[COMMENT]*/                    nv_Renew = NO.  /*A57-0300: §Ò¹µèÍÍÒÂØ*/
/*[COMMENT]*/                    OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[COMMENT]*/                    PUT "PD_SAVEPD1: 1. START " FORMAT "x(25)"
/*[COMMENT]*/                        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    SKIP.
/*[COMMENT]*/                    OUTPUT CLOSE.
/*[COMMENT]*/                    IF IntPol7072.Rencnt <> 0 
/*[COMMENT]*/                       AND SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R"
/*[COMMENT]*/                       AND IntPol7072.PreviousPolicyNumber     <> ""
/*[COMMENT]*/                    THEN nv_Renew = YES.
/*[COMMENT]*/                    nv_msgerror7072  = "".
/*[COMMENT]*/                    /* Generate policy, uwm100, uwm120, uwm301, uwm130, uwd132 */
/*[COMMENT]*/                    IF     IntPol7072.PolicyTypeCd <> "1" 
/*[COMMENT]*/                       AND IntPol7072.PolicyTypeCd <> "2"
/*[COMMENT]*/                       AND IntPol7072.PolicyTypeCd <> "3"
/*[COMMENT]*/                    THEN DO:
/*[COMMENT]*/                      IF    IntPol7072.PolicyTypeCd = ""
/*[COMMENT]*/                         OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[COMMENT]*/                         OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[COMMENT]*/                         /**/
/*[COMMENT]*/                         OR IntPol7072.CMIPolicyTypeCd = "110"
/*[COMMENT]*/                         OR IntPol7072.CMIPolicyTypeCd = "120A"
/*[COMMENT]*/                         OR IntPol7072.CMIPolicyTypeCd = "140A"
/*[COMMENT]*/                      THEN DO:
/*[COMMENT]*/                        IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                          nv_msgerror7072 = "".
/*[COMMENT]*/                          RUN UZ/UZO7201WS.P
/*[COMMENT]*/                               (nv_RecIntPol7072
/*[COMMENT]*/                               ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                          FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                          NO-ERROR.
/*[COMMENT]*/                          IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                            OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[COMMENT]*/                            PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                                TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                                " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                            OUTPUT CLOSE.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END. /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
/*[COMMENT]*/                        IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[COMMENT]*/                           OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[COMMENT]*/                        THEN DO:
/*[COMMENT]*/                          RUN WRS/WRSGU100.P
/*[COMMENT]*/                               (nv_RecIntPol7072
/*[COMMENT]*/                               ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                          IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                            OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[COMMENT]*/                            PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                                TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                                " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                            OUTPUT CLOSE.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ELSE DO:
/*[COMMENT]*/                        IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                          RUN UZ/UZO7201WS.P
/*[COMMENT]*/                               (nv_RecIntPol7072
/*[COMMENT]*/                               ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                          FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                          NO-ERROR.
/*[COMMENT]*/                          IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                            OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[COMMENT]*/                            PUT "4. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                                TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                                " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                            OUTPUT CLOSE.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    ELSE DO:
/*[COMMENT]*/                      /* A57-0300: §Ò¹µèÍÍÒÂØ */
/*[COMMENT]*/                      IF nv_Renew = YES THEN DO:
/*[COMMENT]*/                        IF NOT CONNECTED ("expiry") THEN DO:
/*[COMMENT]*/                          RUN WRS/WRSGU1DB.P
/*[COMMENT]*/                               (""  /*Userid*/
/*[COMMENT]*/                              ,""  /*Password*/
/*[COMMENT]*/                              ,"expiry"). /*Database name*/
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        IF CONNECTED ("expiry") THEN DO:
/*[COMMENT]*/                          RUN WRS/WRSGU10R.P
/*[COMMENT]*/                            (nv_RecIntPol7072
/*[COMMENT]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE  nv_msgerror7072 = "Not Connect Expiry: ".
/*[COMMENT]*/                        /**/
/*[COMMENT]*/                        FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                        NO-ERROR.
/*[COMMENT]*/                        IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                          OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[COMMENT]*/                          PUT "5. WRSGU10R.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                              TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                              " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                          OUTPUT CLOSE.
/*[COMMENT]*/                          nv_msgerror7072 = "".
/*[COMMENT]*/                          RUN WRS/WRSGU100.P
/*[COMMENT]*/                             (nv_RecIntPol7072
/*[COMMENT]*/                             ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                        NO-ERROR.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      ELSE DO:
/*[COMMENT]*/                        IF IntPol7072.PolicyTypeCd <> "1"  THEN DO:
/*[COMMENT]*/                          RUN WRS/WRSGU100.P
/*[COMMENT]*/                             (nv_RecIntPol7072
/*[COMMENT]*/                             ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                          IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.PolicyTypeCd = "1" THEN DO:
/*[COMMENT]*/                            nv_acno1 = "".
/*[COMMENT]*/                            /* ËÒ ÃËÑÊµÑÇá·¹  */
/*[COMMENT]*/                            RUN WSP/WSPMCpny.P 
/*[COMMENT]*/                                (IntPol7072.CompanyCode
/*[COMMENT]*/                                ,IntPol7072.BranchCd
/*[COMMENT]*/                                ,"V" + SUBSTR(IntPol7072.PolicyNumber,3,2) /*nv_Poltyp*/
/*[COMMENT]*/                                ,OUTPUT crCompanyNo
/*[COMMENT]*/                                ,OUTPUT crBranchNo
/*[COMMENT]*/                                ,OUTPUT nv_acno1
/*[COMMENT]*/                                ,OUTPUT nv_agent
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072
/*[COMMENT]*/                                ).
/*[COMMENT]*/                            /**/
/*[COMMENT]*/                            IF nv_acno1 <> "" THEN DO:
/*[COMMENT]*/                              FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                              NO-ERROR.
/*[COMMENT]*/                              IF AVAILABLE IntPol7072 THEN 
/*[COMMENT]*/                                IntPol7072.AgentBrokerLicenseNumber = nv_acno1.
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END.
/*[COMMENT]*/                          ELSE DO:
/*[COMMENT]*/                            RUN WRS/WRSGU100.P
/*[COMMENT]*/                                (nv_RecIntPol7072
/*[COMMENT]*/                                ,INPUT-OUTPUT nv_msgerror7072).
/*[COMMENT]*/                          END.
/*[COMMENT]*/                        END.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                      IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                        OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[COMMENT]*/                        PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                            TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                            " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                        OUTPUT CLOSE.
/*[COMMENT]*/                        FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                        NO-ERROR.
/*[COMMENT]*/                        IF AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                          ASSIGN
/*[COMMENT]*/                          IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[COMMENT]*/                          IntPol7072.GenSicBran   = NO
/*[COMMENT]*/                          IntPol7072.GenSicBranText = nv_msgerror7072
/*[COMMENT]*/                          IntPol7072.GenSicBranST = "ERROR"
/*[COMMENT]*/                          IntPol7072.ErrorMessage = nv_msgerror7072.
/*[COMMENT]*/                          IF IntPol7072.DocumentUID <> "" THEN
/*[COMMENT]*/                             IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        RETURN.
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[COMMENT]*/                    PUT "PD_SAVEPD1: 2. END " FORMAT "x(25)"
/*[COMMENT]*/                        " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                    SKIP.
/*[COMMENT]*/                    OUTPUT CLOSE.
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[COMMENT]*/                    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                    NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                      ASSIGN
/*[COMMENT]*/                      IntPol7072.GenSicBran   = YES
/*[COMMENT]*/                      IntPol7072.GenSicBranBy = "WRSBQ7072.W"
/*[COMMENT]*/                      IntPol7072.GenSicBranDt = TODAY
/*[COMMENT]*/                      IntPol7072.GenSicBranTime = STRING(TIME,"HH:MM:SS")
/*[COMMENT]*/                      IntPol7072.GenSicBranText = ""
/*[COMMENT]*/                      IntPol7072.GenSicBranST = "".
/*[COMMENT]*/                      IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN IntPol7072.TransferToPremium = YES. /*kridtiya i.*/
/*[COMMENT]*/                      IF nv_msgerror7072 <> "" THEN DO:
/*[COMMENT]*/                        ASSIGN
/*[COMMENT]*/                        IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[COMMENT]*/                        IntPol7072.GenSicBran   = NO
/*[COMMENT]*/                        IntPol7072.GenSicBranText = nv_msgerror7072
/*[COMMENT]*/                        IntPol7072.GenSicBranST = "ERROR"
/*[COMMENT]*/                        IntPol7072.ErrorMessage = nv_msgerror7072.
/*[COMMENT]*/                        IF IntPol7072.DocumentUID <> "" THEN
/*[COMMENT]*/                           IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[COMMENT]*/                      END.
/*[COMMENT]*/                    END.
/*[COMMENT]*/                    FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[COMMENT]*/                    NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                    IF AVAILABLE IntPol7072 THEN DO:
/*[COMMENT]*/                      IF IntPol7072.ConfirmBy <> "AUTO" THEN DO:
/*[COMMENT]*/                        /*»2.1 2.2 ·Ø¹µèÓ¡ÇèÒ1áÊ¹ÍÍ¡¡ÃÁ¸ÃÃÁì D ËÃ×ÍàÅ¢ÊÒ¢Ò 2 ËÅÑ¡*/
/*[COMMENT]*/                        IF (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[COMMENT]*/                           AND  nv_okprn = YES
/*[COMMENT]*/                        THEN DO:
/*[COMMENT]*/                          /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                          RUN PD_SAVEPD1FileAttach (nv_RecIntPol7072
/*[COMMENT]*/                                                   ,nv_rec_rq ).
/*[COMMENT]*/                        END.
/*[COMMENT]*/                        ELSE DO:
/*[COMMENT]*/                          OUTPUT TO ConfirmBy.TXT.
/*[COMMENT]*/                          PUT IntPol7072.ConfirmBy SKIP.
/*[COMMENT]*/                          OUTPUT CLOSE.
/*[COMMENT]*/                          IF IntPol7072.ChkVehicle = YES THEN DO:
/*[COMMENT]*/                            nv_resulttext = "Êè§µÃÇ¨ÊÀÒ¾Ã¶".
/*[COMMENT]*/                            nv_cvehtext =
/*[COMMENT]*/                              "µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)
/*[COMMENT]*/                            + " ¡ÃÁ¸ÃÃÁì¾Ãº.: " + TRIM(nv_PolicyV72)
/*[COMMENT]*/                              + " ·ÐàºÕÂ¹Ã¶ : " + TRIM(IntPol7072.Registration) 
/*[COMMENT]*/                                          + " " + TRIM(IntPol7072.RegisteredProvCd)
/*[COMMENT]*/                            + " ¼ÙéàÍÒ»ÃÐ¡Ñ¹ÀÑÂ : " + TRIM(TRIM(TRIM(IntPol7072.InsuredTitle)
/*[COMMENT]*/                            + " " + TRIM(IntPol7072.InsuredName))
/*[COMMENT]*/                            + " " + TRIM(IntPol7072.InsuredSurname)).
/*[COMMENT]*/                            OUTPUT TO ChkVehicle.TXT.
/*[COMMENT]*/                            PUT " ConfirmBy:" IntPol7072.ConfirmBy  SKIP.
/*[COMMENT]*/                            PUT "ChkVehicle:" IntPol7072.ChkVehicle SKIP.
/*[COMMENT]*/                            PUT "           " nv_resulttext FORMAT "X(50)" SKIP.
/*[COMMENT]*/                            PUT "           " nv_cvehtext   FORMAT "X(200)" SKIP.
/*[COMMENT]*/                            PUT "ChkVehSend:" IntPol7072.ChkVehSend SKIP.
/*[COMMENT]*/                            PUT "   Mail to:" IntPol7072.ChkVehMail SKIP.
/*[COMMENT]*/                            PUT "------------------------------------------------" FORMAT "X(50)" SKIP.
/*[COMMENT]*/                            PUT "ChkVehAssignSend: " IntPol7072.ChkVehAssignSend SKIP.
/*[COMMENT]*/                            PUT "ChkVehAssignMail: " IntPol7072.ChkVehAssignMail SKIP.
/*[COMMENT]*/                            OUTPUT CLOSE.
/*[COMMENT]*/                            IF IntPol7072.ChkVehSend = YES THEN DO:
/*[COMMENT]*/                              IF IntPol7072.ChkVehMail <> "" THEN DO:
/*[COMMENT]*/                                RUN GW/gwtomail 
/*[COMMENT]*/                                  (IntPol7072.ChkVehMail /*To "USERNAME"*/
/*[COMMENT]*/                                  ,""          /*CC.*/
/*[COMMENT]*/                                  ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)) /*Subject: WARNING VIB*/
/*[COMMENT]*/                                  ,nv_cvehtext /*Body*/
/*[COMMENT]*/                                  ).
/*[COMMENT]*/                              END.
/*[COMMENT]*/                            END.
/*[COMMENT]*/                            IF IntPol7072.ChkVehAssignSend = YES THEN DO:
/*[COMMENT]*/                              IF IntPol7072.ChkVehAssignMail <> "" THEN DO:
/*[COMMENT]*/                                RUN GW/gwtomail
/*[COMMENT]*/                                  (IntPol7072.ChkVehAssignMail
/*[COMMENT]*/                                  ,""
/*[COMMENT]*/                                  ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70))
/*[COMMENT]*/                                  ,nv_cvehtext
/*[COMMENT]*/                                  ).
/*[COMMENT]*/                              END.
/*[COMMENT]*/                            END.
/*[COMMENT]*/                            IF IntPol7072.TransferToPremium = YES THEN DO: /* Gen Uwm100 to DB GWCtx */
/*[COMMENT]*/                              RUN WRS\WRSGwCtx 
/*[COMMENT]*/                                  (IntPol7072.PolicyNumber
/*[COMMENT]*/                                  ,IntPol7072.Rencnt
/*[COMMENT]*/                                  ,IntPol7072.Endcnt
/*[COMMENT]*/                                  ,0  /*RECID(uwm100)*/
/*[COMMENT]*/                                  ,IntPol7072.CompanyCode  /*833*/
/*[COMMENT]*/                                  ,IntPol7072.PolicyTypeCd /*2.2*/
/*[COMMENT]*/                                  ,IntPol7072.RateGroup).  /*110*/
/*[COMMENT]*/                            END.
/*[COMMENT]*/                          END. /*IF IntS7072.ChkVehicle = YES*/
/*[COMMENT]*/                        END. 
/*[COMMENT]*/                      END. /*IF IntS7072.ConfirmBy <> "AUTO" */
/*[COMMENT]*/                      IF IntPol7072.ConfirmBy = "AUTO" THEN DO:
/*[COMMENT]*/                          IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN 
/*[COMMENT]*/                          RUN WRS\WRSGwCtx 
/*[COMMENT]*/                                  (IntPol7072.PolicyNumber
/*[COMMENT]*/                                  ,IntPol7072.Rencnt
/*[COMMENT]*/                                  ,IntPol7072.Endcnt
/*[COMMENT]*/                                  ,0  /*RECID(uwm100)*/
/*[COMMENT]*/                                  ,IntPol7072.CompanyCode  /*833*/
/*[COMMENT]*/                                  ,IntPol7072.PolicyTypeCd /*2.2*/
/*[COMMENT]*/                                  ,IntPol7072.RateGroup).  /*110*/ /*kridtiya i.*/
/*[COMMENT]*/                          ELSE  /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                             RUN PD_SAVEPD1FileAttach
/*[COMMENT]*/                                (nv_RecIntPol7072
/*[COMMENT]*/                                ,nv_rec_rq ).  /*RECID(IntS7072)*/
/*[COMMENT]*/                        END.
/*[COMMENT]*/                    END.  /*IF AVAILABLE IntPol7072 */
/*[COMMENT]*/                    RELEASE IntS7072.
/*[COMMENT]*/                    RELEASE IntPol7072.
/*[COMMENT]*/                    RELEASE IntQPolicy.
/**/   */
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_R C-Win 
PROCEDURE PD_SAVEPD1_R :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
RUN WRS\WRSGwCtx 
              (IntPol7072.PolicyNumber
              ,IntPol7072.Rencnt
              ,IntPol7072.Endcnt
              ,0  /*RECID(uwm100)*/
              ,IntPol7072.CompanyCode  /*833*/
              ,IntPol7072.PolicyTypeCd /*2.2*/
              ,IntPol7072.RateGroup).  /*110*/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD2 C-Win 
PROCEDURE PD_SAVEPD2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes: PD_SAVEPD1 Êè§ÁÒ      
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
OUTPUT TO PD_SAVEPD2.TXT APPEND.
PUT "PD_SAVEPD2: 1. Start: " FORMAT "x(25)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
SKIP.
OUTPUT CLOSE.
/*[BLANK]*/                      
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    /**/
ASSIGN
IntPol7072.Username          = IntS7072.Username
IntPol7072.Password          = IntS7072.Password
IntPol7072.CompanyCode       = IntS7072.CompanyCode
IntPol7072.BranchCd          = IntS7072.BranchCd
IntPol7072.InsurerId         = IntS7072.InsurerId
IntPol7072.InsuranceCd       = IntS7072.InsuranceCd
IntPol7072.InsuranceBranchCd = IntS7072.InsuranceBranchCd
IntPol7072.PolicyStatus      = IntS7072.PolicyStatus
/*[BLANK]*/                      
IntPol7072.PolicyNumber      = nv_PolicyV70
/*[COMMENT]*/                    /*IntPol7072.DocumentUID       = IntS7072.DocumentUID /*nv_DocnoV70*/*/
/*[BLANK]*/                      
IntPol7072.PreviousPolicyNumber = IntS7072.PreviousPolicyNumber
IntPol7072.StickerNumber        = IntS7072.StickerNumber
/*[BLANK]*/                      
IntPol7072.ContractNumber           = IntS7072.ContractNumber
IntPol7072.ContractDt               = IntS7072.ContractDt
IntPol7072.ContractTime             = IntS7072.ContractTime
IntPol7072.CMVApplicationNumber     = IntS7072.CMVApplicationNumber /*QNumPremium*/
IntPol7072.ApplicationNumber        = IntS7072.ApplicationNumber
IntPol7072.ApplicationDt            = IntS7072.ApplicationDt
IntPol7072.ApplicationTime          = IntS7072.ApplicationTime
IntPol7072.AgreeDt                  = IntS7072.AgreeDt
IntPol7072.IssueDt                  = IntS7072.IssueDt
IntPol7072.EffectiveDt              = IntS7072.EffectiveDt
IntPol7072.ExpirationDt             = IntS7072.ExpirationDt
IntPol7072.EndDt                    = IntS7072.EndDt
IntPol7072.SetTime                  = IntS7072.SetTime
IntPol7072.VehicleTypeCode          = IntS7072.VehicleTypeCode
IntPol7072.VehTypeCd                = IntS7072.VehTypeCd
IntPol7072.RegisteredProvinceCode   = IntS7072.RegisteredProvinceCode
IntPol7072.PlateNumber              = IntS7072.PlateNumber
IntPol7072.Registration             = IntS7072.Registration
IntPol7072.RegisteredProvCd         = IntS7072.RegisteredProvCd
IntPol7072.VehGroup                 = IntS7072.VehGroup
IntPol7072.Manufacturer             = IntS7072.Manufacturer
IntPol7072.Model                    = IntS7072.Model
IntPol7072.ModelTypeName            = IntS7072.ModelTypeName
IntPol7072.VehBodyTypeDesc          = IF       index(IntS7072.VehBodyTypeDesc,"null")  <> 0 THEN "" 
                                      ELSE IF  index(IntS7072.VehBodyTypeDesc,"äÁèÁÕ") <> 0 THEN ""
                                      ELSE    IntS7072.VehBodyTypeDesc  
IntPol7072.ColourCd                 = IntS7072.ColourCd
IntPol7072.Colour                   = IntS7072.Colour
IntPol7072.ModelYear                = IntS7072.ModelYear
IntPol7072.Displacement             = IF IntS7072.Displacement             = "null" THEN "0" ELSE IntS7072.Displacement
IntPol7072.GrossVehOrCombinedWeight = IF IntS7072.GrossVehOrCombinedWeight = "null" THEN "0" ELSE IntS7072.GrossVehOrCombinedWeight
IntPol7072.SeatingCapacity          = IF IntS7072.SeatingCapacity          = "null" THEN "0" ELSE IntS7072.SeatingCapacity 
IntPol7072.ChassisVINNumber         = IntS7072.ChassisVINNumber
IntPol7072.ChassisSerialNumber      = IntS7072.ChassisSerialNumber
IntPol7072.EngineSerialNumber       = IntS7072.EngineSerialNumber
IntPol7072.RegisteredYear           = IntS7072.RegisteredYear
IntPol7072.License                  = IntS7072.License
IntPol7072.InsuredType              = IntS7072.InsuredType
IntPol7072.InsuredUniqueID          = IntS7072.InsuredUniqueID
IntPol7072.InsuredUniqueIDExpDt     = IntS7072.InsuredUniqueIDExpDt
IntPol7072.InsuredUniqueIDExpirationDt = IntS7072.InsuredUniqueIDExpirationDt
IntPol7072.BirthDt           = IF index(IntS7072.BirthDt,"null") = 0 THEN  IntS7072.BirthDt ELSE ""
IntPol7072.InsuredCd         = IntS7072.InsuredCd
/*[COMMENT]*/                    /*IntPol7072.InsuredTitle      = IntS7072.InsuredTitle  Comment by 
/*[COMMENT]*/                    IntPol7072.InsuredTitle      =  IF      trim(IntS7072.InsuredTitle) = "¹ÒÂ"    THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹Ò§ÊÒÇ" THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹Ò§"    THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹.Ê."   THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE    trim(IntS7072.InsuredTitle)*/
IntPol7072.InsuredTitle      = IntS7072.InsuredTitle     /*Add by kridtiya i . Date. 03/09/2018*/
IntPol7072.InsuredName       = IntS7072.InsuredName
IntPol7072.InsuredSurname    = IntS7072.InsuredSurname
IntPol7072.InsuredBranch     = IntS7072.InsuredBranch
IntPol7072.Addr              = IntS7072.Addr
IntPol7072.UnitNumber        = IntS7072.UnitNumber
IntPol7072.RoomNumber        = IntS7072.RoomNumber
IntPol7072.Building          = IntS7072.Building
IntPol7072.VillageNumber     = IntS7072.VillageNumber
IntPol7072.Alley             = IntS7072.Alley
IntPol7072.Lane              = IntS7072.Lane
IntPol7072.StreetName        = IntS7072.StreetName
IntPol7072.SubDistrict       = IntS7072.SubDistrict
IntPol7072.District          = IntS7072.District
IntPol7072.StateProvCd       = IntS7072.StateProvCd
IntPol7072.StateProv         = IntS7072.StateProv
IntPol7072.Province          = IntS7072.Province
IntPol7072.PostalCode        = IntS7072.PostalCode
IntPol7072.OccupationDesc    = IntS7072.OccupationDesc
IntPol7072.MobilePhoneNumber = IntS7072.MobilePhoneNumber
IntPol7072.MobileNumber      = IntS7072.MobileNumber
IntPol7072.PhoneNumber       = IntS7072.PhoneNumber
IntPol7072.OfficePhoneNumber = IntS7072.OfficePhoneNumber
IntPol7072.EmailAddr         = IntS7072.EmailAddr
IntPol7072.WrittenAmt        = IntS7072.WrittenAmt
IntPol7072.RevenueStampAmt   = IntS7072.RevenueStampAmt
IntPol7072.VatAmt            = IntS7072.VatAmt
IntPol7072.CurrentTermAmt    = IntS7072.CurrentTermAmt
IntPol7072.RegisterDt        = IntS7072.RegisterDt
IntPol7072.ShowroomID        = IntS7072.ShowroomID
IntPol7072.ShowroomName      = IntS7072.ShowroomName
IntPol7072.ReceiptDt         = IntS7072.ReceiptDt
IntPol7072.AgencyEmployee    = IntS7072.AgencyEmployee
IntPol7072.RemarkText        = IntS7072.RemarkText
IntPol7072.ReceiptName       = IntS7072.ReceiptName
IntPol7072.ReceiptAddr       = IntS7072.ReceiptAddr
/*[BLANK]*/                      
IntPol7072.AgentBrokerLicenseNumber = IntS7072.AgentBrokerLicenseNumber .
/*[BLANK]*/                      
ASSIGN
IntPol7072.ReferenceNumber      = IntS7072.ReferenceNumber
IntPol7072.EndorseEffectiveDt   = IntS7072.EndorseEffectiveDt
IntPol7072.EndorseRefNumber     = IntS7072.EndorseRefNumber
IntPol7072.EndorseFlag          = IntS7072.EndorseFlag /* Product Code ·Õè Premium */
IntPol7072.SystemRq             = IntS7072.SystemRq
IntPol7072.InsurerCode          = IntS7072.InsurerCode
IntPol7072.MethodCode           = IntS7072.MethodCode
/*[BLANK]*/                      
IntPol7072.Policy               = nv_PolicyV70
IntPol7072.Rencnt               = IntS7072.Rencnt
IntPol7072.Endcnt               = IntS7072.Endcnt
IntPol7072.Riskno               = IntS7072.Riskno
IntPol7072.Itemno               = IntS7072.Itemno
IntPol7072.SendByUser           = IntS7072.SendByUser
IntPol7072.SendDate             = IntS7072.SendDate
IntPol7072.SendTime             = IntS7072.SendTime
/*[BLANK]*/                      
IntPol7072.SystemErrorStatus1   = IntS7072.SystemErrorStatus1
IntPol7072.SystemErrorStatus2   = IntS7072.SystemErrorStatus2
IntPol7072.SystemErrorStatus3   = IntS7072.SystemErrorStatus3
IntPol7072.vehreg               = IntS7072.vehreg
IntPol7072.ReceiveNumber        = IntS7072.ReceiveNumber
IntPol7072.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
IntPol7072.accdat               = IntS7072.accdat
IntPol7072.comdat               = IntS7072.comdat
IntPol7072.expdat               = IntS7072.expdat
IntPol7072.ErrorMessage         = IntS7072.ErrorMessage
/*[BLANK]*/                      
IntPol7072.SumInsureAmt         = IntS7072.SumInsureAmt
/*[BLANK]*/                      
IntPol7072.PolicyTypeCd              = IntS7072.PolicyTypeCd
IntPol7072.RateGroup                 = IntS7072.RateGroup
IntPol7072.GarageTypeCd              = IntS7072.GarageTypeCd
IntPol7072.GarageDesc                = IntS7072.GarageDesc
/*[BLANK]*/                      
IntPol7072.COLLAmtAccident           = IntS7072.COLLAmtAccident
IntPol7072.DeductibleCOLLAmtAccident = IntS7072.DeductibleCOLLAmtAccident 
IntPol7072.FTAmt                     = IntS7072.FTAmt
/*[BLANK]*/                      
IntPol7072.CMIPolicyTypeCd      = IntS7072.CMIPolicyTypeCd
IntPol7072.CMIVehTypeCd         = IntS7072.CMIVehTypeCd
/*[BLANK]*/                      
IntPol7072.CMIPolicyNumber      = nv_PolicyV72
/*[COMMENT]*/                    /*IntPol7072.CMIDocumentUID       = IntS7072.CMIDocumentUID /*nv_DocnoV72*/*/
/*[BLANK]*/                      
IntPol7072.CMIApplicationNumber = IntS7072.CMIApplicationNumber
/*[BLANK]*/                      
/*[COMMENT]*/                    /*IntPol7072.CMIBarCodeNumber     = IntS7072.CMIBarCodeNumber*/
IntPol7072.CMIEffectiveDt       = IntS7072.CMIEffectiveDt
IntPol7072.CMIExpirationDt      = IntS7072.CMIExpirationDt
IntPol7072.CMIAmtPerson         = IntS7072.CMIAmtPerson
IntPol7072.CMIAmtAccident       = IntS7072.CMIAmtAccident
IntPol7072.CMIWrittenAmt        = IntS7072.CMIWrittenAmt
IntPol7072.CMIRevenueStampAmt   = IntS7072.CMIRevenueStampAmt
IntPol7072.CMIVatAmt            = IntS7072.CMIVatAmt
IntPol7072.CMICurrentTermAmt    = IntS7072.CMICurrentTermAmt
IntPol7072.PromptText           = IntS7072.PromptText
IntPol7072.OptionValueDesc      = IntS7072.OptionValueDesc
/*[BLANK]*/                      
IntPol7072.RqUID                = nv_octets
/*[BLANK]*/                      
IntPol7072.keyRequestIndRq      = IntS7072.keyRequestIndRq
IntPol7072.TransactionDateRq    = IntS7072.TransactionDateRq
IntPol7072.TransactionTimeRq    = IntS7072.TransactionTimeRq
IntPol7072.MsgStatusCd          = IntS7072.MsgStatusCd
IntPol7072.BirthDate            = IntS7072.BirthDate
IntPol7072.BirthDate1           = IntS7072.BirthDate1
IntPol7072.BirthDate2           = IntS7072.BirthDate2
IntPol7072.InsUIDExpDate        = IntS7072.InsUIDExpDate
IntPol7072.InsUIDExpDate1       = IntS7072.InsUIDExpDate1
IntPol7072.InsUIDExpDate2       = IntS7072.InsUIDExpDate2
IntPol7072.CMIComDate           = IntS7072.CMIComDate
IntPol7072.CMIExpDate           = IntS7072.CMIExpDate
/*[BLANK]*/                      
/*[COMMENT]*/                    /*02/10/2015*/
IntPol7072.Statusflag = IntS7072.Statusflag /*Flag Ã¶·Ñ¹ã¨ à©¾ÒÐ»ÃÐàÀ· 1*/
IntPol7072.Finint     = IntS7072.Finint     /*Deler / VAT CODE*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
IntPol7072.ResponseResult    = IntS7072.ResponseResult
IntPol7072.ResultStatus      = "SUCCESS"
/*[BLANK]*/                      
IntPol7072.ProcessStatus     = "X"
IntPol7072.ProcessByUser     = IntS7072.ProcessByUser
IntPol7072.ProcessDate       = IntS7072.ProcessDate
IntPol7072.ProcessTime       = IntS7072.ProcessTime
IntPol7072.TrnFromIntByUser  = IntS7072.TrnFromIntByUser
IntPol7072.TrnFromIntDate    = IntS7072.TrnFromIntDate
IntPol7072.TrnFromIntTime    = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
IntPol7072.ConfirmBy         = IntS7072.ConfirmBy
IntPol7072.ByUserID          = IntS7072.ByUserID
IntPol7072.ChkVehicle        = IntS7072.ChkVehicle
IntPol7072.ChkVehBy          = IntS7072.ChkVehBy
IntPol7072.ChkVehDt          = IntS7072.ChkVehDt
IntPol7072.ChkVehTime        = IntS7072.ChkVehTime
IntPol7072.ChkVehSend        = IntS7072.ChkVehSend
IntPol7072.ChkVehMail        = IntS7072.ChkVehMail
/*[BLANK]*/                      
IntPol7072.ChkVehAssignBy    = IntS7072.ChkVehAssignBy
IntPol7072.ChkVehAssignDt    = IntS7072.ChkVehAssignDt
IntPol7072.ChkVehAssignTime  = IntS7072.ChkVehAssignTime
IntPol7072.ChkVehAssignSend  = IntS7072.ChkVehAssignSend
IntPol7072.ChkVehAssignMail  = IntS7072.ChkVehAssignMail
/*[BLANK]*/                      
IntPol7072.TransferToPremium = IntS7072.TransferToPremium .
/*[COMMENT]*/                    /* */
/*[COMMENT]*/                    /* --------------------------------- */
/*[BLANK]*/                      
RUN PD_SAVEPD3. /*Save ÊèÇ¹·ÕèàËÅ×Í*/
/*[COMMENT]*/                    /* --------------------------------- */
/*[BLANK]*/                      
IF TRIM(IntPol7072.CMIDocumentUID) <> "" THEN DO:
/*[BLANK]*/                      
  IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) < 7 THEN DO:
/*[BLANK]*/                        
         IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 6 THEN IntPol7072.CMIDocumentUID = "0" + TRIM(IntPol7072.CMIDocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 5 THEN IntPol7072.CMIDocumentUID = "00" + TRIM(IntPol7072.CMIDocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 4 THEN IntPol7072.CMIDocumentUID = "000" + TRIM(IntPol7072.CMIDocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 3 THEN IntPol7072.CMIDocumentUID = "0000" + TRIM(IntPol7072.CMIDocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 2 THEN IntPol7072.CMIDocumentUID = "00000" + TRIM(IntPol7072.CMIDocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 1 THEN IntPol7072.CMIDocumentUID = "000000" + TRIM(IntPol7072.CMIDocumentUID).
  END.
END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* */
IF TRIM(IntPol7072.DocumentUID) <> "" THEN DO:
/*[BLANK]*/                      
  IF LENGTH(TRIM(IntPol7072.DocumentUID)) < 7 THEN DO:
/*[BLANK]*/                        
         IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 6 THEN IntPol7072.DocumentUID = "0" + TRIM(IntPol7072.DocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 5 THEN IntPol7072.DocumentUID = "00" + TRIM(IntPol7072.DocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 4 THEN IntPol7072.DocumentUID = "000" + TRIM(IntPol7072.DocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 3 THEN IntPol7072.DocumentUID = "0000" + TRIM(IntPol7072.DocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 2 THEN IntPol7072.DocumentUID = "00000" + TRIM(IntPol7072.DocumentUID).
    ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 1 THEN IntPol7072.DocumentUID = "000000" + TRIM(IntPol7072.DocumentUID).
  END.
END.
/*[COMMENT]*/                    /* */
/*[BLANK]*/                      
IF nv_PolicyV70 <> "" THEN DO:
  IF SUBSTR(nv_PolicyV70,1,1) = "Q"  THEN  IntPol7072.Rencnt = 0 . /*Add kridtiya i.§Ò¹ Q ÁÕrenew<>0 */
/*[BLANK]*/                      
  IF IntPol7072.InsuranceBranchCd = "" THEN DO:
/*[COMMENT]*/                           /*D07057SK1234*/
/*[COMMENT]*/                           /*123456789012*/
/*[BLANK]*/                      
    IF    SUBSTR(nv_PolicyV70,1,1) = "D"
       OR SUBSTR(nv_PolicyV70,1,1) = "Q" 
       OR SUBSTR(nv_PolicyV70,1,1) = "R"
    THEN 
         IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,2,1).
/*[BLANK]*/                      
/*[COMMENT]*/                             /*347057C00005*/
/*[COMMENT]*/                             /*123456789012*/
    ELSE IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,1,2).
/*[BLANK]*/                      
  END.
END.
ELSE DO:
  IF nv_PolicyV72 <> "" THEN DO:
/*[BLANK]*/                      
    IF IntPol7072.InsuranceBranchCd = "" THEN DO:
/*[COMMENT]*/                             /*D07057SK1234*/
/*[COMMENT]*/                             /*123456789012*/
      IF    SUBSTR(nv_PolicyV70,1,1) = "D" 
         OR SUBSTR(nv_PolicyV70,1,1) = "Q"
         OR SUBSTR(nv_PolicyV70,1,1) = "R"
      THEN 
           IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,2,1).
/*[BLANK]*/                        
/*[COMMENT]*/                               /*347057C00005*/
/*[COMMENT]*/                               /*123456789012*/
      ELSE IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,1,2).
/*[BLANK]*/                        
    END.
  END.
/*[BLANK]*/                      
END.
/*[COMMENT]*/                    /*First Loss ·Ø¹ 1 áÊ¹ ¡Ñº2 áÊ¹*/
/*[BLANK]*/                      
IF TRIM(IntPol7072.PolicyNumber) <> "" THEN DO:
/*[BLANK]*/                      
  IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" /*AND nv_Poltyp = "V70" */
     AND    IntPol7072.PolicyTypeCd      = "1" 
     AND   (IntPol7072.RateGroup         = "110" OR IntPol7072.RateGroup = "320")
  THEN DO:
    IF     (DECIMAL(IntPol7072.SumInsureAmt) = 100000
        OR  DECIMAL(IntPol7072.SumInsureAmt) = 200000)
       AND (DECIMAL(IntPol7072.CurrentTermAmt) = 9899.64 
        OR  DECIMAL(IntPol7072.CurrentTermAmt) = 12499.74
        OR  DECIMAL(IntPol7072.CurrentTermAmt) = 12000.05)
    THEN DO:
      IntPol7072.EndorseFlag = "FIRST LOSS".  /* Product Code ·Õè Premium */
/*[BLANK]*/                        
      IF TRIM(IntPol7072.ReceiptNumber) = ""  OR TRIM(IntPol7072.ReceiptNumber) = "0" OR
         TRIM(IntPol7072.ReceiptNumber) = "O" 
      THEN DO: /* Promotion ·Õè Premium */
/*[BLANK]*/                        
        IF IntPol7072.RateGroup = "110" THEN IntPol7072.ReceiptNumber = "¤ØéÁ¤èÒ FL110".
        IF IntPol7072.RateGroup = "320" THEN IntPol7072.ReceiptNumber = "¤ØéÁ¤èÒ FL320".
      END.
    END.
  END.
/*[BLANK]*/                      
/*[COMMENT]*/                      /*442 ¤Ø³ÊÁÈÑ¡´Ôì ->à«ç¹·ÃÑÅ ÍÔ¹ªÑÇÃì*/
  IF    (SUBSTR(IntPol7072.PolicyNumber,1,1) <> "Q" 
    AND  SUBSTR(IntPol7072.PolicyNumber,1,1) <> "R")
/*[COMMENT]*/                            /**/
    AND (IntPol7072.PolicyTypeCd = "2.2" OR IntPol7072.PolicyTypeCd = "3.2")
/*[COMMENT]*/                            /**/
    AND (   IntPol7072.RateGroup = "110" OR IntPol7072.RateGroup = "210" 
         OR IntPol7072.RateGroup = "320" )
  THEN DO:
    IF      DECIMAL(IntPol7072.SumInsureAmt) = 100000
/*[COMMENT]*/                                /**/
       AND (DECIMAL(IntPol7072.CurrentTermAmt) = 7254.60 
        OR  DECIMAL(IntPol7072.CurrentTermAmt) = 6608.32 )
    THEN DO:
      IntPol7072.EndorseFlag = "FIX".  /* Product Code ·Õè Premium */
    END.
  END.
END.
/*[BLANK]*/                      
OUTPUT TO PD_SAVEPD2.TXT APPEND.
PUT "PD_SAVEPD2: 2. END: " FORMAT "x(25)"
    " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
SKIP.
OUTPUT CLOSE.
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD3 C-Win 
PROCEDURE PD_SAVEPD3 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
ASSIGN
IntPol7072.DriverNameCd     = IntS7072.DriverNameCd
IntPol7072.InsuredTitle1    = IntS7072.InsuredTitle1
IntPol7072.InsuredName1     = IntS7072.InsuredName1
IntPol7072.InsuredSurname1  = IntS7072.InsuredSurname1
IntPol7072.OccupationDesc1  = IntS7072.OccupationDesc1
IntPol7072.BirthDt1         = IntS7072.BirthDt1
IntPol7072.InsuredUniqueID1 = IntS7072.InsuredUniqueID1
IntPol7072.License1         = IntS7072.License1
IntPol7072.InsuredTitle2    = IntS7072.InsuredTitle2
IntPol7072.InsuredName2     = IntS7072.InsuredName2
IntPol7072.InsuredSurname2  = IntS7072.InsuredSurname2
IntPol7072.OccupationDesc2  = IntS7072.OccupationDesc2
IntPol7072.BirthDt2         = IntS7072.BirthDt2
IntPol7072.InsuredUniqueID2 = IntS7072.InsuredUniqueID2
IntPol7072.License2         = IntS7072.License2
/*[COMMENT]*/                    /**/
IntPol7072.TPBIAmtPerson             = IntS7072.TPBIAmtPerson
IntPol7072.TPBIAmtAccident           = IntS7072.TPBIAmtAccident
IntPol7072.PDAmtAccident             = IntS7072.PDAmtAccident
IntPol7072.DeductiblePDAmtAccident   = IntS7072.DeductiblePDAmtAccident
/*[BLANK]*/                      
IntPol7072.PerilsPADriverAmt              = IntS7072.PerilsPADriverAmt
IntPol7072.PerilsPANumPassengers          = IntS7072.PerilsPANumPassengers
IntPol7072.PerilsPAPassengerAmt           = IntS7072.PerilsPAPassengerAmt
IntPol7072.PerilsPATemporaryDriverAmt     = IntS7072.PerilsPATemporaryDriverAmt
IntPol7072.PerilsPANumTemporaryPassengers = IntS7072.PerilsPANumTemporaryPassengers
IntPol7072.PerilsPATemporaryPassengerAmt  = IntS7072.PerilsPATemporaryPassengerAmt
IntPol7072.PerilsMedicalTreatmentAmt      = IntS7072.PerilsMedicalTreatmentAmt
IntPol7072.PerilsBailBondInsuranceAmt     = IntS7072.PerilsBailBondInsuranceAmt
IntPol7072.PremiumCoverage13Amt           = IntS7072.PremiumCoverage13Amt
IntPol7072.DiscountForNamedDriver         = IntS7072.DiscountForNamedDriver
IntPol7072.DeductibleAmt        = IntS7072.DeductibleAmt
IntPol7072.FleetAmt             = IntS7072.FleetAmt
IntPol7072.GoodDriverIndPct     = IntS7072.GoodDriverIndPct
IntPol7072.GoodDriverIndAmt     = IntS7072.GoodDriverIndAmt
IntPol7072.TotalDiscountsAmt    = IntS7072.TotalDiscountsAmt
IntPol7072.SurchargeFactorAmt   = IntS7072.SurchargeFactorAmt
IntPol7072.PremiumCoverage2Amt  = IntS7072.PremiumCoverage2Amt
IntPol7072.OtherDiscountAmt     = IntS7072.OtherDiscountAmt
IntPol7072.ReceiptNumber        = IntS7072.ReceiptNumber   /*CampaignNumber/PromotionNumber*/
IntPol7072.ReceiptName2         = IntS7072.ReceiptName2
IntPol7072.ReceiptAddr2         = IntS7072.ReceiptAddr2
IntPol7072.Beneficiaries        = IntS7072.Beneficiaries
IntPol7072.PolicyAttachment     = IntS7072.PolicyAttachment
IntPol7072.VehicleUse           = IntS7072.VehicleUse .
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
ASSIGN
IntPol7072.COUNTER_NO                = IntS7072.COUNTER_NO
IntPol7072.TERM_NO                   = IntS7072.TERM_NO
IntPol7072.SERVICE_RUN_NO            = IntS7072.SERVICE_RUN_NO
IntPol7072.RECORD_STATUS             = IntS7072.RECORD_STATUS
IntPol7072.CLIENT_SERVICE_RUNNO      = IntS7072.CLIENT_SERVICE_RUNNO
IntPol7072.ZONE                      = IntS7072.ZONE
IntPol7072.R_SERVICE_RUNN            = IntS7072.R_SERVICE_RUNN
IntPol7072.CANCEL_OPERATING          = IntS7072.CANCEL_OPERATING
IntPol7072.OPERATE_BY_STAFF          = IntS7072.OPERATE_BY_STAFF
IntPol7072.SYSTEM_DATE_TIME          = IntS7072.SYSTEM_DATE_TIME
IntPol7072.USERID_CS                 = IntS7072.USERID_CS
IntPol7072.PASSWORD_CS               = IntS7072.PASSWORD_CS
IntPol7072.SUCCESS                   = IntS7072.SUCCESS
IntPol7072.CODE                      = IntS7072.CODE
IntPol7072.DESC_CS                   = IntS7072.DESC_CS
IntPol7072.METHOD                    = IntS7072.METHOD
IntPol7072.TX_ID                     = IntS7072.TX_ID
IntPol7072.LOG_ID                    = IntS7072.LOG_ID
IntPol7072.VENDOR_ID                 = IntS7072.VENDOR_ID
IntPol7072.SERVICE_ID                = IntS7072.SERVICE_ID
IntPol7072.INSURER_PRODUCT_LINE_CODE = IntS7072.INSURER_PRODUCT_LINE_CODE
IntPol7072.PRODUCT_CODE              = IntS7072.PRODUCT_CODE
IntPol7072.PLAN_CODE                 = IntS7072.PLAN_CODE
IntPol7072.CATEGORY                  = IntS7072.CATEGORY
IntPol7072.BILLING_FREQ              = IntS7072.BILLING_FREQ
IntPol7072.NET_PREMIUM               = IntS7072.NET_PREMIUM
IntPol7072.GROSS_PREMIUM             = IntS7072.GROSS_PREMIUM
IntPol7072.SUM_INSURE                = IntS7072.SUM_INSURE
IntPol7072.PRINT_SLIP                = IntS7072.PRINT_SLIP
IntPol7072.POL_NO                    = IntS7072.POL_NO
IntPol7072.CERT_NO                   = IntS7072.CERT_NO
IntPol7072.OLD_POL_NO                = IntS7072.OLD_POL_NO
IntPol7072.NEW_RENEW                 = IntS7072.NEW_RENEW
IntPol7072.POL_YEAR_SEQ              = IntS7072.POL_YEAR_SEQ
IntPol7072.SALE_DATE                 = IntS7072.SALE_DATE
IntPol7072.EFFECTIVE_DATE            = IntS7072.EFFECTIVE_DATE
IntPol7072.END_DATE                  = IntS7072.END_DATE
IntPol7072.NID_NO                    = IntS7072.NID_NO
IntPol7072.CARD_TITLE                = IntS7072.CARD_TITLE
IntPol7072.CARD_NAME                 = IntS7072.CARD_NAME
IntPol7072.CARD_MNAME                = IntS7072.CARD_MNAME
IntPol7072.CARD_SNAME                = IntS7072.CARD_SNAME
IntPol7072.CARD_DOB                  = IntS7072.CARD_DOB
IntPol7072.CARD_GENDER               = IntS7072.CARD_GENDER
IntPol7072.CARD_ADDRESS              = IntS7072.CARD_ADDRESS
IntPol7072.CARD_SUB_DISTRICT         = IntS7072.CARD_SUB_DISTRICT
IntPol7072.CARD_DISTRICT             = IntS7072.CARD_DISTRICT
IntPol7072.CARD_PROVINCE             = IntS7072.CARD_PROVINCE
IntPol7072.CARD_ISSUE_DATE           = IntS7072.CARD_ISSUE_DATE
IntPol7072.CARD_EXPIRED_DATE         = IntS7072.CARD_EXPIRED_DATE
IntPol7072.TEL_NO                    = IntS7072.TEL_NO
IntPol7072.CURRENT_ADDRESS           = IntS7072.CURRENT_ADDRESS
IntPol7072.CURRENT_SUB_DISTRICT      = IntS7072.CURRENT_SUB_DISTRICT
IntPol7072.CURRENT_DISTRICT          = IntS7072.CURRENT_DISTRICT
IntPol7072.CURRENT_PROVINCE          = IntS7072.CURRENT_PROVINCE
IntPol7072.PAYCODE                   = IntS7072.PAYCODE
IntPol7072.CHASSIS_NO                = IntS7072.CHASSIS_NO
IntPol7072.VEHICLE_REG_DATE          = IntS7072.VEHICLE_REG_DATE
IntPol7072.CAR_REG_NO                = IntS7072.CAR_REG_NO
IntPol7072.CAR_REG_PROVINCE          = IntS7072.CAR_REG_PROVINCE
IntPol7072.VEHICLE_TYPE_CODE         = IntS7072.VEHICLE_TYPE_CODE
IntPol7072.VEHICLE_BODY_TYPE         = IntS7072.VEHICLE_BODY_TYPE
IntPol7072.VEHICLE_MAKE              = IntS7072.VEHICLE_MAKE
IntPol7072.VEHICLE_MODEL             = IntS7072.VEHICLE_MODEL
IntPol7072.VEHICLE_USE_CODE          = IntS7072.VEHICLE_USE_CODE
IntPol7072.ENGINE_CC                 = IntS7072.ENGINE_CC
IntPol7072.USE_AREA                  = IntS7072.USE_AREA
IntPol7072.DRIVER_TITLE_1            = IntS7072.DRIVER_TITLE_1
IntPol7072.DRIVER_NAME_1             = IntS7072.DRIVER_NAME_1
IntPol7072.DRIVER_SURNAME_1          = IntS7072.DRIVER_SURNAME_1
IntPol7072.DRIVER_GENDER_1           = IntS7072.DRIVER_GENDER_1
IntPol7072.DRIVER_AGE_1              = IntS7072.DRIVER_AGE_1
IntPol7072.DRIVER_LICENSE_1          = IntS7072.DRIVER_LICENSE_1
IntPol7072.DRIVER_TITLE_2            = IntS7072.DRIVER_TITLE_2
IntPol7072.DRIVER_NMAE_2             = IntS7072.DRIVER_NMAE_2
IntPol7072.DRIVER_SURNAME_2          = IntS7072.DRIVER_SURNAME_2
IntPol7072.DRIVER_GENDER_2           = IntS7072.DRIVER_GENDER_2
IntPol7072.DRIVER_AGE_2              = IntS7072.DRIVER_AGE_2
IntPol7072.DRIVER_LICENSE_2          = IntS7072.DRIVER_LICENSE_2
IntPol7072.COMP_BARCODE              = IntS7072.COMP_BARCODE
IntPol7072.RECEIVE_NO                = IntS7072.RECEIVE_NO
IntPol7072.INVOICE_NO                = IntS7072.INVOICE_NO
IntPol7072.STAMP_RATE                = IntS7072.STAMP_RATE
IntPol7072.VAT                       = IntS7072.VAT .
/*[COMMENT]*/                    /* ------ */
/*[COMMENT]*/                    /**/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEQGwCtx C-Win 
PROCEDURE PD_SAVEQGwCtx :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**
/*[COMMENT]*/                    DEFINE INPUT PARAMETER nv_QRecIntPol7072  AS RECID NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    DEFINE VARIABLE nv_NewInputKey            AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /* ------------------------------------------------------------ */
/*[BLANK]*/                      
/*[COMMENT]*/                      FIND IntPol7072 WHERE RECID(IntPol7072) = nv_QRecIntPol7072
/*[COMMENT]*/                      NO-LOCK NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RUN wctx\WCtxGW100 (INPUT IntPol7072.PolicyNumber
/*[COMMENT]*/                                         ,INPUT IntPol7072.CompanyCode).
/*[COMMENT]*/                      */
/*[BLANK]*/                      
/*[COMMENT]*/                      nv_NewInputKey = STRING(YEAR(TODAY),"9999")
/*[COMMENT]*/                                     + STRING(MONTH(TODAY),"99")
/*[COMMENT]*/                                     + STRING(DAY(TODAY),"99")
/*[COMMENT]*/                                     + SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12).
/*[COMMENT]*/                      nv_NewInputKey = REPLACE(nv_NewInputKey,":","").
/*[COMMENT]*/                      nv_NewInputKey = REPLACE(nv_NewInputKey,".","").
/*[COMMENT]*/                      /**/
/*[BLANK]*/                      
/*[COMMENT]*/                      CREATE IntQPolicy.
/*[COMMENT]*/                      ASSIGN
/*[COMMENT]*/                      IntQPolicy.SystemRq        = "SGwCtx"
/*[COMMENT]*/                      IntQPolicy.keyRequestIndRq = nv_NewInputKey
/*[COMMENT]*/                      IntQPolicy.ProcessStatus   = ""
/*[COMMENT]*/                      IntQPolicy.Policy          = IntPol7072.PolicyNumber
/*[COMMENT]*/                      IntQPolicy.Rencnt          = IntPol7072.Rencnt
/*[COMMENT]*/                      IntQPolicy.Endcnt          = IntPol7072.Endcnt
/*[COMMENT]*/                      IntQPolicy.PolicyRec       = 0                       /*RECID(uwm100)*/
/*[COMMENT]*/                      IntQPolicy.ProcessByUser   = IntPol7072.CompanyCode
/*[COMMENT]*/                      IntQPolicy.ProcessDate     = TODAY
/*[COMMENT]*/                      IntQPolicy.ProcessTime     = STRING(TIME,"HH:MM:SS") + "." + SUBSTR(STRING(MTIME,">>>>99999999"),10,3) 
/*[COMMENT]*/                      IntQPolicy.PolicyTypeCd    = IntPol7072.PolicyTypeCd
/*[COMMENT]*/                      IntQPolicy.RateGroup       = IntPol7072.RateGroup
/*[COMMENT]*/                      IntQPolicy.Releas          = YES .                  /*uwm100.releas*/
/*[BLANK]*/                      
/*[COMMENT]*/                      OUTPUT TO SavetoIntQPolicy.TXT APPEND.
/*[COMMENT]*/                      PUT TODAY FORMAT "99/99/9999" 
/*[COMMENT]*/                      " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      " " IntPol7072.PolicyNumber FORMAT "x(16)"
/*[COMMENT]*/                          IntPol7072.CompanyCode  FORMAT "x(10)" SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.
/*[COMMENT]*/                    **/
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_FileAttach2 C-Win 
PROCEDURE proc_FileAttach2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*create by Kridtiya i.    Lockton */
DEFINE INPUT        PARAMETER nv_CompanyCode            AS CHARACTER NO-UNDO.              
DEFINE INPUT        PARAMETER nv_PolicyType             AS CHARACTER NO-UNDO. /*v70,v72*/  
DEFINE INPUT        PARAMETER nv_CMIPolicyTypeCd        AS CHARACTER NO-UNDO.  
DEFINE INPUT        PARAMETER nv_RECIDIntPol7072        AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd             AS CHARACTER NO-UNDO.              
DEFINE INPUT-OUTPUT PARAMETER nv_PrgName                AS CHARACTER NO-UNDO.              
DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg                 AS CHARACTER NO-UNDO.  
/*[BLANK]*/                      
DEFINE              VARIABLE  nv_SAVEmsgerror          AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    nv_PolicyType  = "V70".
/*[COMMENT]*/                    nv_CompanyCode = "210".
/*[COMMENT]*/                    nv_CMIPolicyTypeCd = "3".
/*[COMMENT]*/                    */
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
IF IntPol7072.SERVICE_ID = "online" THEN nv_CompanyCode = TRIM(nv_CompanyCode) + "cer".
/*[BLANK]*/                      
ASSIGN
  nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[BLANK]*/                      
  FIND FIRST FNameAttach WHERE
             FNameAttach.CompanyCode  = nv_CompanyCode
         AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
         AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
         AND FNameAttach.EffDate     <= TODAY
         AND FNameAttach.SelectNumber = 2
  NO-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE FNameAttach THEN DO:
      ASSIGN 
          nv_NameCompCd = FNameAttach.CompanyCode
          nv_PrgName    = FNameAttach.PrgName
          nv_PrmPrg     = FNameAttach.PrmPrg.
  END.
/*[COMMENT]*/                      /*IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr709".
  ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /* Wctxr709 */*/
  IF   nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr702_RP".
                       ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /* Wctxr709 */
FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-LOCK NO-ERROR NO-WAIT.
IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[BLANK]*/                      
RUN VALUE(nv_PrgName)
    (IntPol7072.CompanyCode         /*nv_BrokerCompany*/   /*nv_BrokerCompany*/
    ,IntPol7072.CMIPolicyNumber                        
    ,IntPol7072.Rencnt                                 
    ,IntPol7072.Endcnt                                 
    ,IntPol7072.CMIDocumentUID                         
    ,IntPol7072.RqUID               /*nv_code  keyRequestIndRq*/     /*nv_code  keyRequestIndRq*/
    ,""                             /*n_user   */                          /*n_user   */
    ,""                             /*n_passwd */                          
    ,nv_PrmPrg                      /*Name Report*/                        /*n_passwd */
    ,OUTPUT nv_SAVEmsgerror). 
IF IntPol7072.SERVICE_ID = "online" THEN nv_NameCompCd = TRIM(IntPol7072.CompanyCode) + "cer".
/*[BLANK]*/                      
END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/*[BLANK]*/                      
