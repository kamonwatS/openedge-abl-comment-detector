/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[COMMENT]*/                    /* Connected Databases 
/*[COMMENT]*/                              buint            PROGRESS
/*[COMMENT]*/                    */
/*[EXECUTABLE]*/                 &Scoped-define WINDOW-NAME C-Win
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*[EXECUTABLE]*/                 CREATE WIDGET-POOL.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***************************  Definitions  ************************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Parameters Definitions ---                                           */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Local Variable Definitions ---                                       */
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ConfirmBy  AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_rec_rq     AS RECID                               NO-UNDO. /* RECORD ID REQUEST */
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_rec_rs     AS RECID                               NO-UNDO. /* RECORD ID RESPONSE */
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_process    AS CHARACTER                           NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_InsurerCodeRq      AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ConfirmByUserID    AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_UserIDLine         AS INTEGER                             NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_FindUcf            AS INT64                               NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_recUcf             AS INT64                               NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SwtFind            AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE my-datetime           AS CHARACTER FORMAT "X(23)"            NO-UNDO. 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* 2555/09/01 Chek Number Limit Request / Companay*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumLimitRqPerDay   AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumLimitRqPerWeek  AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumLimitRqPerMonth AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumLimitRqPerYear  AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumRqPerDay        AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumRqPerWeek       AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumRqPerMonth      AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NumRqPerYear       AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ManageTo           AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SendTo             AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ForDate            AS DATE      NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Week               AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Month              AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_count              AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PolicyV70          AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_DocnoV70           AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PolicyV72          AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_DocnoV72           AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_msgerror           AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_octets             AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_sendchkvehicle     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_cvehtext           AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_resulttext         AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_RecIntPol7072      AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_msgerror7072       AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_exit               AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Acno1              AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Agent              AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ChkQ               AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Mdocno1           AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Mtrty11           AS CHARACTER NO-UNDO. /*M*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Tdocno1           AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Ttrty11           AS CHARACTER NO-UNDO. /*T*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_DuplPolicy        AS CHARACTER INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LongCount         AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LongTime          AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SWContractNumber  AS CHARACTER INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_IChoice           AS LOGICAL NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SplitQno          AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CountTime         AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CountLong         AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CountLeave        AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*Check data Expiry*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_expiryrencnt      AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_expirysigr_p      AS DECIMAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_expiryprem_t      AS DECIMAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_expirysclass      AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_expirycovcod      AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrnRenew          AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CheckForDB        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE STREAM xmlstream.
/*[COMMENT]*/                    /* */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE TEMP-TABLE TFileAttach NO-UNDO
/*[EXECUTABLE]*/                 FIELD  FileNameAttach         AS CHARACTER
/*[EXECUTABLE]*/                 FIELD  FileBinary             AS BLOB.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------- */
/*[EXECUTABLE]*/                 DEFINE NEW SHARED TEMP-TABLE T-DOCNO  NO-UNDO
/*[EXECUTABLE]*/                 FIELD  CompNo         AS CHARACTER   /*833*/
/*[EXECUTABLE]*/                 FIELD  Branch         AS CHARACTER   /*0*/
/*[EXECUTABLE]*/                 FIELD  RunNo          AS INTEGER     /*100*/
/*[EXECUTABLE]*/                 FIELD  RcpNoStr       AS INTEGER     /*8330501*/
/*[EXECUTABLE]*/                 FIELD  RcpNoEnd       AS INTEGER     /*8330600*/
/*[EXECUTABLE]*/                 FIELD  EntDate        AS DATE        /*01/01/14*/
/*[EXECUTABLE]*/                 FIELD  RcpFlg         AS CHARACTER   /*T, M*/
/*[EXECUTABLE]*/                 FIELD  UseRecp        AS INTEGER     /*¨Ó¹Ç¹àºÍÃì·Õè¶Ù¡ãªéä»*/
/*[EXECUTABLE]*/                 FIELD  NextNo         AS INTEGER     /*àºÍÃì¤ÃÑé§µèÍä»*/
/*[EXECUTABLE]*/                 FIELD  UseFlag        AS CHARACTER   /*"", FULL*/
/*[COMMENT]*/                    /* */
/*[EXECUTABLE]*/                 INDEX T-DOCNO01 IS PRIMARY CompNo  ASCENDING /*833*/
/*[EXECUTABLE]*/                                            RcpFlg  ASCENDING /*T, M*/
/*[EXECUTABLE]*/                                            EntDate ASCENDING /*01/01/14*/
/*[EXECUTABLE]*/                                            UseFlag ASCENDING /*ÇèÒ§ / Full*/
/*[EXECUTABLE]*/                 .
/*[EXECUTABLE]*/                 DEF VAR nv_covcodtyp1   AS CHAR FORMAT "x(5)"  INIT "". /*Add Kridtiya i. Date. 21/01/2016 */
/*[EXECUTABLE]*/                 DEF VAR nv_CompanyCode1 AS CHAR FORMAT "x(10)" INIT "". /*Add Kridtiya i. Date. 14/10/2016 */
/*[EXECUTABLE]*/                 DEF VAR nv_timein       AS CHAR FORMAT "x(10)" INIT "".
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEF VAR nv_ContractNum AS CHAR INIT "".
/*[EXECUTABLE]*/                 DEF VAR nv_docrun2      AS  CHAR.    /* Invoice  */
/*[EXECUTABLE]*/                 DEF VAR nv_docrun3      AS  CHAR.    /* Sticker  */
/*[COMMENT]*/                    /*DESCENDING.*/
/*[COMMENT]*/                    /* ------------------------------------------- */
/*[BLANK]*/                      
/*[COMMENT]*/                    /************************************************************************/
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ********************  Preprocessor Definitions  ******************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define PROCEDURE-TYPE Window
/*[EXECUTABLE]*/                 &Scoped-define DB-AWARE no
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Name of designated FRAME-NAME and/or first browse and/or first query */
/*[EXECUTABLE]*/                 &Scoped-define FRAME-NAME DEFAULT-FRAME
/*[EXECUTABLE]*/                 &Scoped-define BROWSE-NAME br_QueueRequest
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Internal Tables (found by Frame, Query & Browse Queries)             */
/*[EXECUTABLE]*/                 &Scoped-define INTERNAL-TABLES IntS7072
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions for BROWSE br_QueueRequest                               */
/*[EXECUTABLE]*/                 &Scoped-define FIELDS-IN-QUERY-br_QueueRequest /* */ IntS7072.ProcessStatus IntS7072.TrnFromIntDate IntS7072.TrnFromIntTime IntS7072.CompanyCode IntS7072.ContractNumber /* IntS7072.PolicyNumber */ IntS7072.PolicyTypeCd IntS7072.RateGroup IntS7072.DocumentUID /**/ IntS7072.CMIPolicyTypeCd IntS7072.CMIVehTypeCd IntS7072.CMIBarCodeNumber /* IntS7072.comdat IntS7072.expdat IntS7072.Registration IntS7072.RegisteredProvCd IntS7072.CMIComDate IntS7072.CMIExpDate */ /* END. */   
/*[EXECUTABLE]*/                 &Scoped-define ENABLED-FIELDS-IN-QUERY-br_QueueRequest   
/*[EXECUTABLE]*/                 &Scoped-define SELF-NAME br_QueueRequest
/*[EXECUTABLE]*/                 &Scoped-define OPEN-QUERY-br_QueueRequest /* OPEN QUERY {&SELF-NAME}     FOR EACH IntS7072 WHERE              IntS7072.SystemRq   = fi_ResponseJob NO-LOCK. */ IF fi_RequestorRq = "ALL" THEN DO:   /* 22/9   FIND FIRST IntS7072 WHERE              IntS7072.SystemRq   = fi_ResponseJob      /* "SPmotor" */          AND IntS7072.MethodCode = STRING(fi_SplitQno) /*22/9*/   NO-LOCK NO-ERROR NO-WAIT.   */   OPEN QUERY {&SELF-NAME}       FOR EACH IntS7072 WHERE                IntS7072.SystemRq   = fi_ResponseJob            AND IntS7072.MethodCode = nv_SplitQno    /*22/9*/    NO-LOCK. END. ELSE DO:   /* 22/9   FIND FIRST IntS7072 WHERE              IntS7072.SystemRq    = fi_ResponseJob     /* "SPmotor" */         AND  IntS7072.CompanyCode = fi_RequestorRq     /* "KK", ~
/*[COMMENT]*/                           à¡ÕÂÃµÔ¹Ò¤Ô¹ */         AND  IntS7072.MethodCode  = STRING(fi_SplitQno) /*22/9*/   NO-LOCK NO-ERROR NO-WAIT.   */   OPEN QUERY {&SELF-NAME}       FOR EACH IntS7072 WHERE                IntS7072.SystemRq    = fi_ResponseJob            AND IntS7072.CompanyCode = fi_RequestorRq /* "KK", ~
/*[EXECUTABLE]*/                        à¡ÕÂÃµÔ¹Ò¤Ô¹ */            AND IntS7072.MethodCode  = nv_SplitQno    /*22/9*/   NO-LOCK.  END.
/*[EXECUTABLE]*/                 &Scoped-define TABLES-IN-QUERY-br_QueueRequest IntS7072
/*[EXECUTABLE]*/                 &Scoped-define FIRST-TABLE-IN-QUERY-br_QueueRequest IntS7072
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions for FRAME DEFAULT-FRAME                                  */
/*[EXECUTABLE]*/                 &Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
/*[EXECUTABLE]*/                     ~{&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Standard List Definitions                                            */
/*[EXECUTABLE]*/                 &Scoped-Define ENABLED-OBJECTS fi_ResponseJob fi_RequestorRq fi_SplitQno ~
/*[EXECUTABLE]*/                 buOK buCANCEL br_QueueRequest fi_RequestorName fi_notfound fi_notfound2 ~
/*[EXECUTABLE]*/                 fi_PolicyNumber fi_vehreg fi_Vehicle fi_RegisteredProvinceCode ~
/*[EXECUTABLE]*/                 fi_PlateNumber fi_waitcount rd_SystemExt fi_TextRemark RECT-4 RECT-5 
/*[EXECUTABLE]*/                 &Scoped-Define DISPLAYED-OBJECTS fi_ResponseJob fi_RequestorRq fi_SplitQno ~
/*[EXECUTABLE]*/                 fi_RequestorName fi_notfound fi_notfound2 fi_PolicyNumber fi_vehreg ~
/*[EXECUTABLE]*/                 fi_Vehicle fi_RegisteredProvinceCode fi_PlateNumber fi_waitcount ~
/*[EXECUTABLE]*/                 rd_SystemExt fi_TextRemark 
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Custom List Definitions                                              */
/*[COMMENT]*/                    /* List-1,List-2,List-3,List-4,List-5,List-6                            */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-PREPROCESSOR-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***********************  Control Definitions  ********************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Define the widget handle for the window                              */
/*[EXECUTABLE]*/                 DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Definitions of the field level widgets                               */
/*[EXECUTABLE]*/                 DEFINE BUTTON buCANCEL 
/*[EXECUTABLE]*/                      LABEL "Cancel" 
/*[EXECUTABLE]*/                      SIZE 10 BY 1
/*[EXECUTABLE]*/                      FONT 6.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE BUTTON buOK 
/*[EXECUTABLE]*/                      LABEL "OK" 
/*[EXECUTABLE]*/                      SIZE 10 BY 1
/*[EXECUTABLE]*/                      FONT 6.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_notfound AS CHARACTER FORMAT "X(75)":U 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 78 BY .67
/*[EXECUTABLE]*/                      BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_notfound2 AS CHARACTER FORMAT "X(78)":U 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 78 BY .67
/*[EXECUTABLE]*/                      BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_PlateNumber AS CHARACTER FORMAT "X(15)":U 
/*[EXECUTABLE]*/                      LABEL "·º.Ã¶" 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 10 BY .62
/*[EXECUTABLE]*/                      BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_PolicyNumber AS CHARACTER FORMAT "X(40)":U 
/*[EXECUTABLE]*/                      LABEL "Pol." 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 14 BY .62
/*[EXECUTABLE]*/                      BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_RegisteredProvinceCode AS CHARACTER FORMAT "X(8)":U 
/*[EXECUTABLE]*/                      LABEL "¨Ç.·º.Ã¶" 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 5 BY .62
/*[EXECUTABLE]*/                      BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_RequestorName AS CHARACTER FORMAT "X(45)":U 
/*[EXECUTABLE]*/                      LABEL "(ALL=·Ñé§ËÁ´)" 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 35 BY .71
/*[EXECUTABLE]*/                      BGCOLOR 15  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_RequestorRq AS CHARACTER FORMAT "X(10)":U 
/*[EXECUTABLE]*/                      LABEL "ÃËÑÊºÃÔÉÑ· Broker" 
/*[EXECUTABLE]*/                      VIEW-AS FILL-IN 
/*[EXECUTABLE]*/                      SIZE 10 BY .81
/*[EXECUTABLE]*/                      FGCOLOR 1 FONT 6 NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_ResponseJob AS CHARACTER FORMAT "X(10)":U 
/*[EXECUTABLE]*/                      LABEL "Queue Receive Name" 
/*[EXECUTABLE]*/                      VIEW-AS FILL-IN 
/*[EXECUTABLE]*/                      SIZE 15 BY .81
/*[EXECUTABLE]*/                      FONT 6 NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_SplitQno AS INTEGER FORMAT ">>9":U INITIAL 0 
/*[EXECUTABLE]*/                      LABEL "Split Process Job Queue no." 
/*[EXECUTABLE]*/                      VIEW-AS FILL-IN 
/*[EXECUTABLE]*/                      SIZE 5 BY .81
/*[EXECUTABLE]*/                      FGCOLOR 2 FONT 6 NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_TextRemark AS CHARACTER FORMAT "X(100)":U 
/*[EXECUTABLE]*/                      VIEW-AS FILL-IN 
/*[EXECUTABLE]*/                      SIZE 93 BY .71
/*[EXECUTABLE]*/                      FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_Vehicle AS CHARACTER FORMAT "X(30)":U 
/*[EXECUTABLE]*/                      LABEL "Model" 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 20 BY .62
/*[EXECUTABLE]*/                      BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_vehreg AS CHARACTER FORMAT "X(15)":U 
/*[EXECUTABLE]*/                      LABEL "¡·.Ã¶" 
/*[EXECUTABLE]*/                       VIEW-AS TEXT 
/*[EXECUTABLE]*/                      SIZE 12 BY .62
/*[EXECUTABLE]*/                      BGCOLOR 15 FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE fi_waitcount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
/*[EXECUTABLE]*/                      LABEL "Wait count" 
/*[EXECUTABLE]*/                      VIEW-AS FILL-IN 
/*[EXECUTABLE]*/                      SIZE 15 BY .81 NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE rd_SystemExt AS INTEGER 
/*[EXECUTABLE]*/                      VIEW-AS RADIO-SET HORIZONTAL
/*[EXECUTABLE]*/                      RADIO-BUTTONS 
/*[EXECUTABLE]*/                           "Test", 1,
/*[EXECUTABLE]*/                 "PD", 2,
/*[EXECUTABLE]*/                 "Oth", 3
/*[EXECUTABLE]*/                      SIZE 20 BY .71
/*[EXECUTABLE]*/                      FGCOLOR 2  NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE RECTANGLE RECT-4
/*[EXECUTABLE]*/                      EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
/*[EXECUTABLE]*/                      SIZE 91 BY .19.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE RECTANGLE RECT-5
/*[EXECUTABLE]*/                      EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
/*[EXECUTABLE]*/                      SIZE 91 BY .19.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE RECTANGLE RECT-6
/*[EXECUTABLE]*/                      EDGE-PIXELS 2 GRAPHIC-EDGE    
/*[EXECUTABLE]*/                      SIZE 96.17 BY 1.1
/*[EXECUTABLE]*/                      BGCOLOR 3 .
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Query definitions                                                    */
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND
/*[EXECUTABLE]*/                 DEFINE QUERY br_QueueRequest FOR 
/*[EXECUTABLE]*/                       IntS7072 SCROLLING.
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Browse definitions                                                   */
/*[EXECUTABLE]*/                 DEFINE BROWSE br_QueueRequest
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_QueueRequest C-Win _FREEFORM
/*[EXECUTABLE]*/                   QUERY br_QueueRequest DISPLAY
/*[COMMENT]*/                          /* */
/*[EXECUTABLE]*/                 IntS7072.ProcessStatus          COLUMN-LABEL "ST"            FORMAT "xx"
/*[EXECUTABLE]*/                 IntS7072.TrnFromIntDate         COLUMN-LABEL "Tran.Date"     FORMAT "99/99/9999"
/*[EXECUTABLE]*/                 IntS7072.TrnFromIntTime         COLUMN-LABEL "Tran.Time"     FORMAT "X(08)"      
/*[EXECUTABLE]*/                 IntS7072.CompanyCode            COLUMN-LABEL "CompCode"      FORMAT "X(08)"
/*[EXECUTABLE]*/                 IntS7072.ContractNumber         COLUMN-LABEL "Contract no."  FORMAT "X(16)"
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    IntS7072.PolicyNumber           COLUMN-LABEL "Policy no."    FORMAT "X(14)" */
/*[EXECUTABLE]*/                 IntS7072.PolicyTypeCd           COLUMN-LABEL "PType"         FORMAT "X(4)"
/*[EXECUTABLE]*/                 IntS7072.RateGroup              COLUMN-LABEL "Type"          FORMAT "X(5)"
/*[EXECUTABLE]*/                 IntS7072.DocumentUID            COLUMN-LABEL "Docno."        FORMAT "X(8)"
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 IntS7072.CMIPolicyTypeCd        COLUMN-LABEL "CmiT"          FORMAT "X(4)"
/*[EXECUTABLE]*/                 IntS7072.CMIVehTypeCd           COLUMN-LABEL "CmiV"          FORMAT "X(5)"
/*[EXECUTABLE]*/                 IntS7072.CMIBarCodeNumber       COLUMN-LABEL "StickerNo."    FORMAT "X(13)"
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
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[EXECUTABLE]*/                     WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 5
/*[EXECUTABLE]*/                          FGCOLOR 1 
/*[EXECUTABLE]*/                          TITLE FGCOLOR 1 "Queue ÃÑº¢éÍÁÙÅ V70, V72 Policy à¢éÒÃÐºº - áÅÐµÍº¡ÅÑº" ROW-HEIGHT-CHARS .65.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ************************  Frame Definitions  *********************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE FRAME DEFAULT-FRAME
/*[EXECUTABLE]*/                      fi_ResponseJob AT ROW 2.38 COL 23.33 COLON-ALIGNED WIDGET-ID 8
/*[EXECUTABLE]*/                      fi_RequestorRq AT ROW 3.24 COL 23.33 COLON-ALIGNED WIDGET-ID 28
/*[EXECUTABLE]*/                      fi_SplitQno AT ROW 4.1 COL 23.33 COLON-ALIGNED WIDGET-ID 178
/*[EXECUTABLE]*/                      buOK AT ROW 2.43 COL 86.5 WIDGET-ID 10
/*[EXECUTABLE]*/                      buCANCEL AT ROW 3.76 COL 86.5 WIDGET-ID 12
/*[EXECUTABLE]*/                      br_QueueRequest AT ROW 8.67 COL 1.5 WIDGET-ID 200
/*[EXECUTABLE]*/                      fi_RequestorName AT ROW 3.33 COL 45.83 COLON-ALIGNED WIDGET-ID 30
/*[EXECUTABLE]*/                      fi_notfound AT ROW 7.24 COL 10.17 COLON-ALIGNED NO-LABEL WIDGET-ID 20
/*[EXECUTABLE]*/                      fi_notfound2 AT ROW 7.91 COL 10.17 COLON-ALIGNED NO-LABEL WIDGET-ID 24
/*[EXECUTABLE]*/                      fi_PolicyNumber AT ROW 6.19 COL 81 COLON-ALIGNED WIDGET-ID 100
/*[EXECUTABLE]*/                      fi_vehreg AT ROW 6.19 COL 37.17 COLON-ALIGNED WIDGET-ID 102
/*[EXECUTABLE]*/                      fi_Vehicle AT ROW 6.19 COL 56.17 COLON-ALIGNED WIDGET-ID 168
/*[EXECUTABLE]*/                      fi_RegisteredProvinceCode AT ROW 6.19 COL 8.5 COLON-ALIGNED WIDGET-ID 170
/*[EXECUTABLE]*/                      fi_PlateNumber AT ROW 6.19 COL 20.17 COLON-ALIGNED WIDGET-ID 172
/*[EXECUTABLE]*/                      fi_waitcount AT ROW 4.1 COL 58 COLON-ALIGNED WIDGET-ID 180
/*[EXECUTABLE]*/                      rd_SystemExt AT ROW 2.38 COL 63 NO-LABEL WIDGET-ID 64
/*[EXECUTABLE]*/                      fi_TextRemark AT ROW 5 COL 3 NO-LABEL WIDGET-ID 182
/*[EXECUTABLE]*/                      "                      Queue ÃÑº¢éÍÁÙÅ V70, V72 ¨Ò¡ DB BUExt à¢éÒ-Êè§ÃÑº¡ÅÑº" VIEW-AS TEXT
/*[EXECUTABLE]*/                           SIZE 65.5 BY .71 AT ROW 1.33 COL 31.5 WIDGET-ID 166
/*[EXECUTABLE]*/                           FGCOLOR 4 FONT 6
/*[EXECUTABLE]*/                      " Execute on Gateway Server" VIEW-AS TEXT
/*[EXECUTABLE]*/                           SIZE 29.17 BY .71 AT ROW 1.33 COL 2.33 WIDGET-ID 164
/*[EXECUTABLE]*/                           FGCOLOR 4 FONT 6
/*[EXECUTABLE]*/                      RECT-6 AT ROW 1.14 COL 1.5 WIDGET-ID 162
/*[EXECUTABLE]*/                      RECT-4 AT ROW 7 COL 4 WIDGET-ID 138
/*[EXECUTABLE]*/                      RECT-5 AT ROW 5.86 COL 4 WIDGET-ID 158
/*[EXECUTABLE]*/                     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
/*[EXECUTABLE]*/                          SIDE-LABELS NO-UNDERLINE THREE-D 
/*[EXECUTABLE]*/                          AT COL 1 ROW 1
/*[EXECUTABLE]*/                          SIZE 97.17 BY 12.81 WIDGET-ID 100.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* *********************** Procedure Settings ************************ */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/*[COMMENT]*/                    /* Settings for THIS-PROCEDURE
/*[COMMENT]*/                       Type: Window
/*[COMMENT]*/                       Allow: Basic,Browse,DB-Fields,Window,Query
/*[COMMENT]*/                       Other Settings: COMPILE
/*[COMMENT]*/                     */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME _END-PROCEDURE-SETTINGS
/*[BLANK]*/                      
/*[COMMENT]*/                    /* *************************  Create Window  ************************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _CREATE-WINDOW
/*[EXECUTABLE]*/                 IF SESSION:DISPLAY-TYPE = "GUI":U THEN
/*[EXECUTABLE]*/                   CREATE WINDOW C-Win ASSIGN
/*[EXECUTABLE]*/                          HIDDEN             = YES
/*[EXECUTABLE]*/                          TITLE              = "Safety Insurance Public Company Limited"
/*[EXECUTABLE]*/                          HEIGHT             = 12.62
/*[EXECUTABLE]*/                          WIDTH              = 97
/*[EXECUTABLE]*/                          MAX-HEIGHT         = 45.81
/*[EXECUTABLE]*/                          MAX-WIDTH          = 213.33
/*[EXECUTABLE]*/                          VIRTUAL-HEIGHT     = 45.81
/*[EXECUTABLE]*/                          VIRTUAL-WIDTH      = 213.33
/*[EXECUTABLE]*/                          RESIZE             = yes
/*[EXECUTABLE]*/                          SCROLL-BARS        = no
/*[EXECUTABLE]*/                          STATUS-AREA        = no
/*[EXECUTABLE]*/                          BGCOLOR            = ?
/*[EXECUTABLE]*/                          FGCOLOR            = ?
/*[EXECUTABLE]*/                          KEEP-FRAME-Z-ORDER = yes
/*[EXECUTABLE]*/                          THREE-D            = yes
/*[EXECUTABLE]*/                          MESSAGE-AREA       = no
/*[EXECUTABLE]*/                          SENSITIVE          = yes.
/*[EXECUTABLE]*/                 ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
/*[EXECUTABLE]*/                 IF NOT C-Win:LOAD-ICON("WIMAGE/safety.ico":U) THEN
/*[EXECUTABLE]*/                     MESSAGE "Unable to load icon: WIMAGE/safety.ico"
/*[EXECUTABLE]*/                             VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/*[EXECUTABLE]*/                 &ENDIF
/*[COMMENT]*/                    /* END WINDOW DEFINITION                                                */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***********  Runtime Attributes and AppBuilder Settings  *********** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/*[COMMENT]*/                    /* SETTINGS FOR WINDOW C-Win
/*[COMMENT]*/                      VISIBLE,,RUN-PERSISTENT                                               */
/*[COMMENT]*/                    /* SETTINGS FOR FRAME DEFAULT-FRAME
/*[COMMENT]*/                       FRAME-NAME Custom                                                    */
/*[COMMENT]*/                    /* BROWSE-TAB br_QueueRequest buCANCEL DEFAULT-FRAME */
/*[COMMENT]*/                    /* SETTINGS FOR FILL-IN fi_TextRemark IN FRAME DEFAULT-FRAME
/*[COMMENT]*/                       ALIGN-L                                                              */
/*[COMMENT]*/                    /* SETTINGS FOR RECTANGLE RECT-6 IN FRAME DEFAULT-FRAME
/*[COMMENT]*/                       NO-ENABLE                                                            */
/*[EXECUTABLE]*/                 IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
/*[EXECUTABLE]*/                 THEN C-Win:HIDDEN = no.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _RUN-TIME-ATTRIBUTES-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Setting information for Queries and Browse Widgets fields            */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_QueueRequest
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
/*[EXECUTABLE]*/                 */  /* BROWSE br_QueueRequest */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                       
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ************************  Control Triggers  ************************ */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define SELF-NAME C-Win
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
/*[EXECUTABLE]*/                 ON END-ERROR OF C-Win /* Safety Insurance Public Company Limited */
/*[EXECUTABLE]*/                 OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
/*[COMMENT]*/                      /* This case occurs when the user presses the "Esc" key.
/*[COMMENT]*/                         In a persistently run window, just ignore this.  If we did not, the
/*[COMMENT]*/                         application would exit. */
/*[EXECUTABLE]*/                   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
/*[EXECUTABLE]*/                 ON WINDOW-CLOSE OF C-Win /* Safety Insurance Public Company Limited */
/*[EXECUTABLE]*/                 DO:
/*[COMMENT]*/                      /* This event will close the window and terminate the procedure.  */
/*[EXECUTABLE]*/                   APPLY "CLOSE":U TO THIS-PROCEDURE.
/*[EXECUTABLE]*/                   RETURN NO-APPLY.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define SELF-NAME buCANCEL
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buCANCEL C-Win
/*[EXECUTABLE]*/                 ON CHOOSE OF buCANCEL IN FRAME DEFAULT-FRAME /* Cancel */
/*[EXECUTABLE]*/                 DO:
/*[EXECUTABLE]*/                   Apply "Close" To This-Procedure.  /* »Ô´â»Ãá¡ÃÁ */
/*[EXECUTABLE]*/                   Return No-Apply.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define SELF-NAME buOK
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buOK C-Win
/*[EXECUTABLE]*/                 ON CHOOSE OF buOK IN FRAME DEFAULT-FRAME /* OK */
/*[EXECUTABLE]*/                 DO:
/*[COMMENT]*/                      /* */
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                   fi_ResponseJob   = INPUT fi_ResponseJob
/*[EXECUTABLE]*/                   fi_waitcount     = INPUT fi_waitcount
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      fi_RequestorRq   = INPUT fi_RequestorRq*/
/*[EXECUTABLE]*/                   fi_SplitQno      = INPUT fi_SplitQno
/*[EXECUTABLE]*/                   fi_RequestorName = ""
/*[EXECUTABLE]*/                   fi_RequestorRq   = "ALL".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF fi_SplitQno = 0 THEN DO:
/*[EXECUTABLE]*/                     nv_IChoice = NO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     MESSAGE "äÁèµéÍ§¡ÒÃÃÐºØ Split Queue"     SKIP(1)
/*[EXECUTABLE]*/                             "â»Ã´Â×¹ÂÑ¹¡ÒÃ´Óà¹Ô¹¡ÒÃ"    SKIP
/*[EXECUTABLE]*/                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
/*[EXECUTABLE]*/                     UPDATE nv_IChoice.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_IChoice = NO THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       APPLY "ENTRY" TO fi_SplitQno.
/*[EXECUTABLE]*/                       RETURN NO-APPLY.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF fi_RequestorRq <> "ALL" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF fi_RequestorRq = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       APPLY "ENTRY" TO fi_RequestorRq.
/*[EXECUTABLE]*/                       RETURN NO-APPLY.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST InsurerCode WHERE InsurerCode.InsurerCd = fi_RequestorRq
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE InsurerCode THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       MESSAGE "Not found Insurer Code:" fi_RequestorRq SKIP
/*[EXECUTABLE]*/                               "on Table InsurerCode" SKIP (1) 
/*[EXECUTABLE]*/                       VIEW-AS ALERT-BOX.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       APPLY "ENTRY" TO fi_RequestorRq.
/*[EXECUTABLE]*/                       RETURN NO-APPLY.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     fi_RequestorName = InsurerCode.InsurerName.
/*[EXECUTABLE]*/                     nv_InsurerCodeRq = fi_RequestorRq.
/*[EXECUTABLE]*/                   END. 
/*[COMMENT]*/                      /* */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   nv_ConfirmBy = "Auto".
/*[COMMENT]*/                                                  /* Check Confirm Response data AUTO or USERID */
/*[EXECUTABLE]*/                   FIND FIRST  FConfResponse WHERE
/*[EXECUTABLE]*/                               FConfResponse.SystemConfirm = fi_ResponseJob
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[COMMENT]*/                                                    /* AUTO or USERID */
/*[EXECUTABLE]*/                   IF AVAILABLE  FConfResponse THEN nv_ConfirmBy = FConfResponse.ConfirmBy.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF fi_SplitQno = 0 THEN nv_SplitQno = "".
/*[EXECUTABLE]*/                                      ELSE nv_SplitQno = STRING(fi_SplitQno).
/*[COMMENT]*/                      /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   DISPLAY fi_RequestorName fi_SplitQno WITH FRAME DEFAULT-FRAME.
/*[EXECUTABLE]*/                   DISABLE buOK    buCANCEL WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                     nv_rec_rs  = 0 
/*[EXECUTABLE]*/                     nv_rec_rq  = 0
/*[EXECUTABLE]*/                     nv_SwtFind = "NEXT"
/*[EXECUTABLE]*/                     nv_FindUcf = 0        /*Record find TABLE UserID Confirm*/
/*[EXECUTABLE]*/                     nv_NumLimitRqPerDay   = 0
/*[EXECUTABLE]*/                     nv_ManageTo           = NO
/*[EXECUTABLE]*/                     nv_NumRqPerDay        = 0
/*[EXECUTABLE]*/                     nv_NumRqPerWeek       = 0
/*[EXECUTABLE]*/                     nv_NumRqPerMonth      = 0
/*[EXECUTABLE]*/                     nv_NumRqPerYear       = 0
/*[EXECUTABLE]*/                     nv_ForDate            = TODAY
/*[EXECUTABLE]*/                     nv_Month              = MONTH(TODAY)
/*[EXECUTABLE]*/                     nv_CountLeave         = 0.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ****************************************************************************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   loop_job:
/*[EXECUTABLE]*/                   REPEAT:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     RUN PD_ClearData.
/*[EXECUTABLE]*/                     RUN PD_DispData.
/*[EXECUTABLE]*/                     RUN PD_DispMess ("").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST IntS7072 WHERE 
/*[EXECUTABLE]*/                                IntS7072.SystemRq   = fi_ResponseJob      /* "SPmotor" */
/*[EXECUTABLE]*/                            AND IntS7072.MethodCode = nv_SplitQno
/*[COMMENT]*/                             /*AND IntS7072.MethodCode = STRING(fi_SplitQno) /*22/9*/ */
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
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
/*[EXECUTABLE]*/                     IF NOT AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                       HIDE MESSAGE NO-PAUSE.
/*[EXECUTABLE]*/                       ASSIGN nv_process = "" 
/*[EXECUTABLE]*/                       nv_timein  = STRING(TIME,"HH:MM:SS").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN Pc_CheckDataExt (INPUT nv_ConfirmBy, INPUT-OUTPUT nv_process).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN PD_DispMess ("Please wait check data db external (DMZ).").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_process = "" THEN DO:
/*[COMMENT]*/                               /*      22.00.00¹.      05.00.00¹.*/
/*[EXECUTABLE]*/                         IF TIME >= 79200 OR TIME <= 18000 THEN DO:
/*[EXECUTABLE]*/                           nv_CountLeave = 0.
/*[COMMENT]*/                              /**/
/*[EXECUTABLE]*/                           PAUSE 1 NO-MESSAGE. /*ÊÓËÃÑº F4 ÍÍ¡ä´é*/
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         ELSE DO:
/*[BLANK]*/                      
/*[COMMENT]*/                              /**/
/*[EXECUTABLE]*/                           IF nv_CountLeave <= 10 THEN DO:
/*[BLANK]*/                        
/*[EXECUTABLE]*/                             nv_CountTime = 0.
/*[EXECUTABLE]*/                             nv_CountLong = 1000000.
/*[EXECUTABLE]*/                             nv_CountLong = 500000.   /*1/4ÇÔ¹Ê·Õ*/
/*[EXECUTABLE]*/                             DO  WHILE nv_CountTime <= nv_CountLong:
/*[EXECUTABLE]*/                               nv_CountTime = nv_CountTime + 1.
/*[EXECUTABLE]*/                             END.
/*[BLANK]*/                        
/*[EXECUTABLE]*/                             nv_CountLeave = nv_CountLeave + 1.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           ELSE DO:
/*[EXECUTABLE]*/                             nv_CountLeave = 0.
/*[COMMENT]*/                                /**/
/*[EXECUTABLE]*/                             PAUSE 1 NO-MESSAGE. /*ÊÓËÃÑº F4 ÍÍ¡ä´é*/
/*[COMMENT]*/                              /**/
/*[EXECUTABLE]*/                           END. /**/
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       IF LASTKEY = KEYCODE("F4") THEN LEAVE loop_job.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       NEXT loop_job.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF AVAILABLE IntS7072 THEN nv_rec_rq = RECID(IntS7072).
/*[EXECUTABLE]*/                     ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       CLOSE QUERY br_QueueRequest.
/*[EXECUTABLE]*/                       nv_rec_rq = 0.
/*[EXECUTABLE]*/                       NEXT loop_job.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     CLOSE QUERY br_QueueRequest.
/*[EXECUTABLE]*/                     {&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE IntS7072 THEN NEXT loop_job.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     fi_PolicyNumber           = IntS7072.PolicyNumber
/*[EXECUTABLE]*/                     fi_Vehicle                = TRIM(IntS7072.Manufacturer) + " " + IntS7072.Model
/*[EXECUTABLE]*/                     fi_vehreg                 = IntS7072.vehreg
/*[EXECUTABLE]*/                     fi_RegisteredProvinceCode = IntS7072.RegisteredProvinceCode
/*[EXECUTABLE]*/                     fi_PlateNumber            = IntS7072.PlateNumber
/*[BLANK]*/                          
/*[EXECUTABLE]*/                      .
/*[EXECUTABLE]*/                     RUN PD_DispData.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     nv_count = 0.
/*[EXECUTABLE]*/                     DO  WHILE nv_count <= fi_waitcount:
/*[EXECUTABLE]*/                       nv_count = nv_count + 1.
/*[EXECUTABLE]*/                     END.
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
/*[EXECUTABLE]*/                     IF     IntS7072.ProcessStatus = "C"
/*[EXECUTABLE]*/                        OR  IntS7072.ProcessStatus = "E"
/*[EXECUTABLE]*/                        OR  IntS7072.ProcessStatus = "X"
/*[EXECUTABLE]*/                     THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       CLOSE QUERY br_QueueRequest.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN Pd_DeleteResultX (INPUT-OUTPUT nv_rec_rq).
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       CLOSE QUERY br_QueueRequest.
/*[EXECUTABLE]*/                       {&OPEN-QUERY-br_QueueRequest}
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RELEASE EntS7072Result.
/*[EXECUTABLE]*/                       RELEASE EntS7072.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          RELEASE IntS7072Result.
/*[COMMENT]*/                          */
/*[EXECUTABLE]*/                       RELEASE IntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RELEASE IntPol7072Result.
/*[EXECUTABLE]*/                       RELEASE IntPolicy.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_rec_rq <> 0 THEN PAUSE 1 NO-MESSAGE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF LASTKEY = KEYCODE("F4") THEN LEAVE loop_job.
/*[EXECUTABLE]*/                       NEXT loop_job.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /* ------------------------------- */
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[EXECUTABLE]*/                     IF     IntS7072.ProcessStatus = "C"
/*[EXECUTABLE]*/                        OR  IntS7072.ProcessStatus = "E"
/*[EXECUTABLE]*/                        OR  IntS7072.ProcessStatus = "X"  THEN NEXT loop_job.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF      IntS7072.ProcessStatus = "O" 
/*[EXECUTABLE]*/                     THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          /* ÃÐºØãËéÁÕ¡ÒÃµÃÇ¨ÊÀÒ¾Ã¶ */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       nv_PolicyV70 = ""
/*[EXECUTABLE]*/                       nv_DocnoV70  = ""
/*[COMMENT]*/                          /**/       
/*[EXECUTABLE]*/                       nv_PolicyV72 = ""
/*[EXECUTABLE]*/                       nv_DocnoV72  = ""
/*[EXECUTABLE]*/                       nv_msgerror  = ""
/*[EXECUTABLE]*/                       nv_CompanyCode1 = "".  /*Add Kridtiya i. date. 14/10/2016 */
/*[COMMENT]*/                          /*IF (IntS7072.CompanyCode  = "834") AND (IntS7072.SERVICE_ID = "" ) THEN DO:  /*Add Kridtiya i. date. 14/10/2016 */
/*[COMMENT]*/                              IF IntS7072.PolicyTypeCd <> "" THEN nv_CompanyCode1 = "834Old".   
/*[COMMENT]*/                              ELSE nv_CompanyCode1 = "834".   
/*[COMMENT]*/                          END.                                         /*Add Kridtiya i. date. 14/10/2016 */
/*[EXECUTABLE]*/                       ELSE nv_CompanyCode1 = IntS7072.CompanyCode.                                 /*Add Kridtiya i. date. 14/10/2016 */*/
/*[EXECUTABLE]*/                           nv_CompanyCode1 = IntS7072.CompanyCode.    /*Add Kridtiya i. date. 30/06/2017 */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF IntS7072.ConfirmBy <> "AUTO" THEN DO: 
/*[BLANK]*/                              
/*[COMMENT]*/                            /*QUOTATION NUMBER ÊÓËÃÑºµÃÇ¨ÊÀÒ¾Ã¶*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF IntS7072.PolicyTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                           nv_ChkQ = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           FIND FIRST FUtilSetUp WHERE
/*[EXECUTABLE]*/                                      FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
/*[EXECUTABLE]*/                                  AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
/*[EXECUTABLE]*/                                  AND FUtilSetUp.KeyUtilGrp2  = "Chk"
/*[EXECUTABLE]*/                                  AND FUtilSetUp.KeyUtilGrp3  = "Q"
/*[EXECUTABLE]*/                                  AND FUtilSetUp.KeyUtilGrp4  = "Policy"
/*[EXECUTABLE]*/                                  AND FUtilSetUp.KeyUtilGrp5  = "ALL"         /*833*/
/*[EXECUTABLE]*/                                  AND FUtilSetUp.EffDate     <= TODAY
/*[EXECUTABLE]*/                           NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                           IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             FIND FIRST FUtilSetUp WHERE
/*[EXECUTABLE]*/                                        FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp2  = "Chk"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp3  = "Q"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp4  = "Policy"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp5  = IntS7072.CompanyCode
/*[EXECUTABLE]*/                                    AND FUtilSetUp.EffDate     <= TODAY
/*[EXECUTABLE]*/                             NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                             IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[EXECUTABLE]*/                           END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF nv_ChkQ = "YES" THEN DO:
/*[BLANK]*/                                   
/*[COMMENT]*/                                /*RUN WSP/WSPRunP1WS.P  (INPUT  IntS7072.CompanyCode*/  /*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                             RUN WSP/WSPRunP1WS.P  (INPUT  nv_CompanyCode1           /*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                   ,INPUT  "V70"  /*¢éÒ§ã¹ NEW=Q*/
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.PreviousPolicyNumber
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.PolicyStatus
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.PolicyTypeCd
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.SumInsureAmt
/*[COMMENT]*/                                                       /**/
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_PolicyV70 /*nv_Policy1*/
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_msgerror).
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           ELSE DO:
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
/*[EXECUTABLE]*/                             OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
/*[EXECUTABLE]*/                             PUT "1. v70 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
/*[EXECUTABLE]*/                             OUTPUT CLOSE.
/*[COMMENT]*/                                /*RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode*//*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                             RUN WSP/WSPRunP2WS.P  (INPUT  nv_CompanyCode1         /*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                   ,INPUT  "V70"
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_PolicyV70 /*nv_PolicyV70*/
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_msgerror).
/*[EXECUTABLE]*/                           END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF TRIM(nv_msgerror) <> "" OR TRIM(nv_PolicyV70) = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             IF nv_msgerror = "" THEN
/*[EXECUTABLE]*/                                nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Quotation Policy ¢Í§ Company code: "
/*[EXECUTABLE]*/                                            + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                                  
/*[EXECUTABLE]*/                             OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                             PUT "1. " nv_msgerror FORMAT "X(250)" SKIP.
/*[EXECUTABLE]*/                             OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN PD_DispData.
/*[EXECUTABLE]*/                             RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                PAUSE 5.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             NEXT loop_job.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF IntS7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                              /*
/*[COMMENT]*/                              RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
/*[EXECUTABLE]*/                           OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
/*[EXECUTABLE]*/                           PUT "2. v72 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
/*[EXECUTABLE]*/                           OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
/*[EXECUTABLE]*/                                                 ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                 ,INPUT  "V72"        /*QV72*/
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_PolicyV72
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF TRIM(nv_msgerror) <> "" OR TRIM(nv_PolicyV72) = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             IF nv_msgerror = "" THEN
/*[EXECUTABLE]*/                                nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Quotation Policy ¾Ãº. ¢Í§ Company code: "
/*[EXECUTABLE]*/                                            + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                             PUT "2. " nv_msgerror FORMAT "X(250)" SKIP.
/*[EXECUTABLE]*/                             OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN PD_DispData.
/*[EXECUTABLE]*/                             RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                PAUSE 5. */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             NEXT loop_job.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* ÊÔé¹ÊØ´  ÃÐºØãËéÁÕ¡ÒÃµÃÇ¨ÊÀÒ¾Ã¶ */
/*[COMMENT]*/                          /* ------------------------------- */
/*[EXECUTABLE]*/                       ELSE DO:
/*[BLANK]*/                             
/*[EXECUTABLE]*/                         IF IntS7072.ConfirmBy = "AUTO" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF IntS7072.PolicyTypeCd <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             nv_ChkQ = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             FIND FIRST FUtilSetUp WHERE
/*[EXECUTABLE]*/                                        FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp2  = "Chk"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp3  = "Q"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp4  = "Policy"
/*[EXECUTABLE]*/                                    AND FUtilSetUp.KeyUtilGrp5  = "ALL"         /*833*/
/*[EXECUTABLE]*/                                    AND FUtilSetUp.EffDate     <= TODAY
/*[EXECUTABLE]*/                             NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                             IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               FIND FIRST FUtilSetUp WHERE
/*[EXECUTABLE]*/                                          FUtilSetUp.UtilGrp      = "UTGRP02"     /*UtGrp01*/
/*[EXECUTABLE]*/                                      AND FUtilSetUp.KeyUtilGrp1  = "V70"         /*V70,V72*/
/*[EXECUTABLE]*/                                      AND FUtilSetUp.KeyUtilGrp2  = "Chk"
/*[EXECUTABLE]*/                                      AND FUtilSetUp.KeyUtilGrp3  = "Q"
/*[EXECUTABLE]*/                                      AND FUtilSetUp.KeyUtilGrp4  = "Policy"
/*[EXECUTABLE]*/                                      AND FUtilSetUp.KeyUtilGrp5  = IntS7072.CompanyCode
/*[EXECUTABLE]*/                                      AND FUtilSetUp.EffDate     <= TODAY
/*[EXECUTABLE]*/                               NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                               IF AVAILABLE FUtilSetUp THEN nv_ChkQ = FUtilSetUp.UtilGrpCd1.
/*[EXECUTABLE]*/                             END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             IF nv_ChkQ = "YES" THEN DO:
/*[COMMENT]*/                                  /*RUN WSP/WSPRunP1WS.P  (INPUT  IntS7072.CompanyCode */ /*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                               RUN WSP/WSPRunP1WS.P  (INPUT  nv_CompanyCode1           /*Add Kridtiya i. date. 14/10/2016 */  
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                     ,INPUT  "V70"  /*¢éÒ§ã¹ NEW=Q*/
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.PreviousPolicyNumber
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.PolicyStatus
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.PolicyTypeCd
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.SumInsureAmt
/*[COMMENT]*/                                                         /**/
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_PolicyV70 /*nv_Policy1*/
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_msgerror).
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             ELSE DO:
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
/*[EXECUTABLE]*/                               OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
/*[EXECUTABLE]*/                               PUT "3. v70 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
/*[EXECUTABLE]*/                               OUTPUT CLOSE.
/*[BLANK]*/                                    
/*[EXECUTABLE]*/                               RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
/*[EXECUTABLE]*/                                                     ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                     ,INPUT  "V70"
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_PolicyV70
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_msgerror).
/*[EXECUTABLE]*/                             END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             IF nv_msgerror <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               IF nv_msgerror = "" THEN
/*[EXECUTABLE]*/                                  nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Policy ¢Í§ Company code: "
/*[EXECUTABLE]*/                                              + IntS7072.CompanyCode + IntS7072.Username.
/*[EXECUTABLE]*/                               OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                               PUT "3. " nv_msgerror FORMAT "X(250)" SKIP.
/*[EXECUTABLE]*/                               OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               RUN PD_DispData.
/*[EXECUTABLE]*/                               RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  PAUSE 5. */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               NEXT loop_job.
/*[EXECUTABLE]*/                             END.          
/*[EXECUTABLE]*/                           END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF IntS7072.CMIPolicyTypeCd <> "" THEN DO:
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN WSP/WSPRunPolWS.P (INPUT  IntS7072.CompanyCode*/
/*[EXECUTABLE]*/                             OUTPUT TO WRSBQ7072-CMI.TXT APPEND.
/*[EXECUTABLE]*/                             PUT "4. v72 " IntS7072.CompanyCode IntS7072.BranchCd SKIP.
/*[EXECUTABLE]*/                             OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN WSP/WSPRunP2WS.P  (INPUT  IntS7072.CompanyCode
/*[EXECUTABLE]*/                                                   ,INPUT  IntS7072.BranchCd
/*[EXECUTABLE]*/                                                   ,INPUT  "V72"
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_PolicyV72
/*[EXECUTABLE]*/                                                   ,OUTPUT nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             IF nv_msgerror <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               IF nv_msgerror = "" THEN
/*[EXECUTABLE]*/                                  nv_msgerror = "äÁè¾º¢éÍÁÙÅ Running Policy ¾Ãº. ¢Í§ Company code: "
/*[EXECUTABLE]*/                                              + IntS7072.CompanyCode + IntS7072.Username.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               OUTPUT TO WRSBQ7072-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                               PUT "4. " nv_msgerror FORMAT "X(250)" SKIP.
/*[EXECUTABLE]*/                               OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               RUN PD_ErrorRunPol (nv_rec_rq, nv_msgerror).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               RUN PD_DispData.
/*[EXECUTABLE]*/                               RUN PD_DispMess (nv_msgerror).
/*[COMMENT]*/                                  /*
/*[COMMENT]*/                                  PAUSE 5. */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               NEXT loop_job.
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                           END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN Pc_SAVEProduction (nv_rec_rq
/*[EXECUTABLE]*/                                             ,nv_PolicyV70 /*,nv_DocnoV70*/
/*[EXECUTABLE]*/                                             ,nv_PolicyV72 /*,nv_DocnoV72*/
/*[EXECUTABLE]*/                                             ).
/*[EXECUTABLE]*/                       OUTPUT TO LogTimeInOut7072.TXT APPEND.
/*[EXECUTABLE]*/                       PUT "Log Time:" 
/*[EXECUTABLE]*/                           TODAY FORMAT "99/99/9999" " Time: "  STRING(TIME,"HH:MM:SS") "-" nv_timein    
/*[EXECUTABLE]*/                           " Company:"  nv_CompanyCode1 "Policy:" nv_PolicyV70 nv_PolicyV72 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                       OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RELEASE EntS7072Result.
/*[EXECUTABLE]*/                       RELEASE EntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RELEASE IntPol7072Result.
/*[EXECUTABLE]*/                       RELEASE IntPolicy.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /* --------------------------------------------------- */
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   END.   /* l o o p _ j o b : */
/*[COMMENT]*/                      /* ****************************************************************************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RELEASE EntS7072Result.
/*[EXECUTABLE]*/                   RELEASE EntS7072.
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RELEASE IntS7072Result.
/*[COMMENT]*/                      */
/*[EXECUTABLE]*/                   RELEASE IntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RELEASE IntPol7072Result.
/*[EXECUTABLE]*/                   RELEASE IntPolicy.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   CLOSE QUERY br_QueueRequest.
/*[EXECUTABLE]*/                   fi_notfound  = "".
/*[EXECUTABLE]*/                   fi_notfound2 = "".
/*[EXECUTABLE]*/                   DISPLAY fi_notfound fi_notfound2 WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   ENABLE  buOK   buCANCEL  WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   APPLY "ENTRY" TO fi_ResponseJob.
/*[EXECUTABLE]*/                   RETURN NO-APPLY.
/*[EXECUTABLE]*/                 END. /* DO: */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define SELF-NAME fi_RequestorRq
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_RequestorRq C-Win
/*[EXECUTABLE]*/                 ON LEAVE OF fi_RequestorRq IN FRAME DEFAULT-FRAME /* ÃËÑÊºÃÔÉÑ· Broker */
/*[EXECUTABLE]*/                 DO:
/*[EXECUTABLE]*/                   fi_RequestorRq = INPUT fi_RequestorRq.
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
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &Scoped-define BROWSE-NAME br_QueueRequest
/*[EXECUTABLE]*/                 &UNDEFINE SELF-NAME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ***************************  Main Block  *************************** */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/*[EXECUTABLE]*/                 ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
/*[EXECUTABLE]*/                        THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* The CLOSE event can be used from inside or outside the procedure to  */
/*[COMMENT]*/                    /* terminate it.                                                        */
/*[EXECUTABLE]*/                 ON CLOSE OF THIS-PROCEDURE 
/*[EXECUTABLE]*/                    RUN disable_UI.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Best default for GUI applications is...                              */
/*[EXECUTABLE]*/                 PAUSE 0 BEFORE-HIDE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* Now enable the interface and wait for the exit condition.            */
/*[COMMENT]*/                    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
/*[COMMENT]*/                    /* --- */
/*[EXECUTABLE]*/                 CLEAR  ALL     NO-PAUSE.
/*[EXECUTABLE]*/                 STATUS INPUT   OFF.
/*[EXECUTABLE]*/                 HIDE   MESSAGE NO-PAUSE.
/*[COMMENT]*/                    /* ------------------------------------------------------------------ */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* --- */
/*[EXECUTABLE]*/                 MAIN-BLOCK:
/*[EXECUTABLE]*/                 DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
/*[EXECUTABLE]*/                    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*[BLANK]*/                       
/*[COMMENT]*/                      /********************  T I T L E   F O R  C - W I N  ****************/
/*[BLANK]*/                        
/*[EXECUTABLE]*/                   DEF  VAR  gv_prgid   AS   CHAR  FORMAT "X(8)"  NO-UNDO.
/*[EXECUTABLE]*/                   DEF  VAR  gv_prog    AS   CHAR  FORMAT "X(40)" NO-UNDO.
/*[EXECUTABLE]*/                   gv_prgid = "WRSBQ7072".
/*[EXECUTABLE]*/                   gv_prog  = "Queue Get & Send data Policy Motor".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN  WSU\WSUHDExt ({&WINDOW-NAME}:handle,gv_prgid,gv_prog).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN  WUT\WUTWICEN (C-WIN:handle).  
/*[COMMENT]*/                      /*********************************************************************/
/*[COMMENT]*/                      /* */
/*[EXECUTABLE]*/                   SESSION:DATA-ENTRY-RETURN = YES.      /* ÃÑº¤èÒ»ØèÁ ENTER */
/*[BLANK]*/                        
/*[COMMENT]*/                      /* ãÊè¤èÒµÑÇá»ÃáÅÐáÊ´§¤èÒ */
/*[EXECUTABLE]*/                   fi_RequestorRq = "ALL".
/*[EXECUTABLE]*/                   fi_ResponseJob = "SPmotor".
/*[EXECUTABLE]*/                   fi_SplitQno = 7.
/*[EXECUTABLE]*/                   rd_SystemExt = 2.
/*[EXECUTABLE]*/                   fi_TextRemark = "M82 1107 1018 834 1470 1146 2117 1554 ".
/*[EXECUTABLE]*/                   DISPLAY fi_RequestorRq fi_ResponseJob WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN enable_UI.
/*[EXECUTABLE]*/                   IF NOT THIS-PROCEDURE:PERSISTENT THEN
/*[EXECUTABLE]*/                     WAIT-FOR CLOSE OF THIS-PROCEDURE.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /* **********************  Internal Procedures  *********************** */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
/*[EXECUTABLE]*/                 PROCEDURE disable_UI :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     DISABLE the User Interface
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       Here we clean-up the user-interface by deleting
/*[COMMENT]*/                                   dynamic widgets we have created and/or hide 
/*[COMMENT]*/                                   frames.  This procedure is usually called when
/*[COMMENT]*/                                   we are ready to "clean-up" after running.
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                      /* Delete the WINDOW we created */
/*[EXECUTABLE]*/                   IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
/*[EXECUTABLE]*/                   THEN DELETE WIDGET C-Win.
/*[EXECUTABLE]*/                   IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
/*[EXECUTABLE]*/                 PROCEDURE enable_UI :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     ENABLE the User Interface
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       Here we display/view/enable the widgets in the
/*[COMMENT]*/                                   user-interface.  In addition, OPEN all queries
/*[COMMENT]*/                                   associated with each FRAME and BROWSE.
/*[COMMENT]*/                                   These statements here are based on the "Other 
/*[COMMENT]*/                                   Settings" section of the widget Property Sheets.
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                   DISPLAY fi_ResponseJob fi_RequestorRq fi_SplitQno fi_RequestorName fi_notfound 
/*[EXECUTABLE]*/                           fi_notfound2 fi_PolicyNumber fi_vehreg fi_Vehicle 
/*[EXECUTABLE]*/                           fi_RegisteredProvinceCode fi_PlateNumber fi_waitcount rd_SystemExt 
/*[EXECUTABLE]*/                           fi_TextRemark 
/*[EXECUTABLE]*/                       WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
/*[EXECUTABLE]*/                   ENABLE fi_ResponseJob fi_RequestorRq fi_SplitQno buOK buCANCEL 
/*[EXECUTABLE]*/                          br_QueueRequest fi_RequestorName fi_notfound fi_notfound2 
/*[EXECUTABLE]*/                          fi_PolicyNumber fi_vehreg fi_Vehicle fi_RegisteredProvinceCode 
/*[EXECUTABLE]*/                          fi_PlateNumber fi_waitcount rd_SystemExt fi_TextRemark RECT-4 RECT-5 
/*[EXECUTABLE]*/                       WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
/*[EXECUTABLE]*/                   {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
/*[EXECUTABLE]*/                   VIEW C-Win.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_CheckDataExt C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_CheckDataExt :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       (INPUT nv_ConfirmBy, INPUT-OUTPUT nv_process).
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_ConfirmBy AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_process   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_RECIDEntS7072 AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_msgerrordupl  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_user          AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errort        AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PolicyTypeCd  AS CHARACTER INITIAL "" NO-UNDO.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 nv_RECIDEntS7072 = 0
/*[EXECUTABLE]*/                 nv_process       = ""
/*[EXECUTABLE]*/                 nv_msgerrordupl  = ""
/*[EXECUTABLE]*/                 nv_Acno1   = ""
/*[EXECUTABLE]*/                 nv_Agent   = ""
/*[EXECUTABLE]*/                 nv_Mdocno1 = ""
/*[EXECUTABLE]*/                 nv_Mtrty11 = ""
/*[EXECUTABLE]*/                 nv_Tdocno1 = ""
/*[EXECUTABLE]*/                 nv_Ttrty11 = ""
/*[EXECUTABLE]*/                 nv_DuplPolicy = "".
/*[EXECUTABLE]*/                   FIND FIRST EntS7072  WHERE
/*[EXECUTABLE]*/                              EntS7072.SystemRq      = fi_ResponseJob /* "SPmotor" */
/*[EXECUTABLE]*/                          AND EntS7072.ProcessStatus = ""             /* ""=New , "O"=Get data*/
/*[EXECUTABLE]*/                          AND EntS7072.MethodCode    = nv_SplitQno    /*22/9*/
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE EntS7072 THEN DO:
/*[EXECUTABLE]*/                     nv_LongCount = 0.
/*[EXECUTABLE]*/                     nv_RECIDEntS7072 = RECID(EntS7072).
/*[EXECUTABLE]*/                     IF EntS7072.CompanyCode <> "1146" THEN RUN Pc_SAVECKAGT (INPUT TRIM(EntS7072.CompanyCode)
/*[EXECUTABLE]*/                                                                             ,INPUT-OUTPUT nv_msgerrordupl).
/*[EXECUTABLE]*/                     IF nv_msgerrordupl  = "" THEN RUN Pc_SAVECKGrp (INPUT nv_RECIDEntS7072
/*[EXECUTABLE]*/                                                                    ,INPUT-OUTPUT nv_msgerrordupl ).
/*[COMMENT]*/                        /*Add Check blacklist by Kridtiya i. Date. 27/01/2018*/
/*[EXECUTABLE]*/                     IF nv_msgerrordupl  = "" THEN RUN Pc_SAVEFMRate (INPUT nv_RECIDEntS7072   
/*[EXECUTABLE]*/                                                                      ,INPUT-OUTPUT nv_msgerrordupl ).
/*[EXECUTABLE]*/                     IF ( nv_msgerrordupl  = "" ) THEN DO:   /*AND ( EntS7072.PolicyStatus = "N" ) THEN DO:*/
/*[EXECUTABLE]*/                         IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO:  /* 70 */ 
/*[EXECUTABLE]*/                           RUN wrs/WRSGUCKLTWS           /* WRSGUCKLT.P  */      
/*[EXECUTABLE]*/                               (INPUT  trim(trim(EntS7072.Registration) + " " + trim(EntS7072.RegisteredProvCd)) /*nn_vehreglist*/  
/*[EXECUTABLE]*/                               ,INPUT  trim(EntS7072.InsuredName)                                                /*nn_namelist */
/*[EXECUTABLE]*/                               ,INPUT  trim(EntS7072.InsuredSurname) 
/*[EXECUTABLE]*/                               ,INPUT  trim(EntS7072.ChassisSerialNumber)                                        /*nv_chanolist*/
/*[EXECUTABLE]*/                               ,INPUT  trim(EntS7072.InsuredUniqueID)                                            /*nv_idnolist*/
/*[EXECUTABLE]*/                               ,OUTPUT nv_msgerrordupl).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /*Add Check blacklist by Kridtiya i. Date. 27/01/2018*/
/*[EXECUTABLE]*/                     IF nv_msgerrordupl  = "" THEN DO:
/*[COMMENT]*/                             /*ISUZU*/
/*[EXECUTABLE]*/                       IF     EntS7072.PolicyStatus          = "R"
/*[EXECUTABLE]*/                          AND EntS7072.PreviousPolicyNumber <> ""
/*[EXECUTABLE]*/                          AND EntS7072.PolicyTypeCd         <> ""
/*[COMMENT]*/                           /*AND EntS7072.CompanyCode           = "476" /*ISUZU*/ *//*comment by Kridtiya i. 17/06/2017*/
/*[COMMENT]*/                          /* AND SUBSTR(EntS7072.PolicyNumber,1,2) = "74" */
/*[EXECUTABLE]*/                       THEN DO:
/*[EXECUTABLE]*/                         ASSIGN 
/*[EXECUTABLE]*/                         nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
/*[EXECUTABLE]*/                         nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew     = "".
/*[EXECUTABLE]*/                         IF NOT CONNECTED ("expiry") THEN DO:
/*[EXECUTABLE]*/                           RUN WRS/WRSGU1DB.P
/*[EXECUTABLE]*/                                (""         /*Userid*/
/*[EXECUTABLE]*/                                ,""         /*Password*/
/*[EXECUTABLE]*/                                ,"expiry"). /*Database name*/
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         IF CONNECTED ("expiry") THEN DO:   /*Check ·Ø¹ /àºÕéÂ /covcod ·Õè expiry µÃ§¡Ñ¹ËÃ×ÍäÁè*/
/*[EXECUTABLE]*/                             RUN WRS/WRSGUCKE.P ("EXT"           /*DB BUExt*/
/*[EXECUTABLE]*/                                                ,RECID(EntS7072)
/*[COMMENT]*/                                                    /**/
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_expiryrencnt /*expiry.uwm100.rencnt*/
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_expirysigr_p
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_expiryprem_t
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_expirysclass
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_expirycovcod
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_PrnRenew
/*[EXECUTABLE]*/                                                ,INPUT-OUTPUT nv_msgerrordupl).
/*[COMMENT]*/                                /*
/*[COMMENT]*/                                RUN PD_PUTError ("EX1"). */
/*[EXECUTABLE]*/                             IF nv_PrnRenew = "YES" THEN nv_msgerrordupl = "". /*¢éÍÁÙÅ expiry à·èÒ¡Ñ¹*/
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF EntS7072.PolicyStatus = "R"*/
/*[EXECUTABLE]*/                     END. /*IF nv_msgerrordupl  = "" THEN DO:*/
/*[EXECUTABLE]*/                     IF nv_msgerrordupl  = "" THEN DO:
/*[EXECUTABLE]*/                       IF (EntS7072.CompanyCode = "570" OR EntS7072.CompanyCode = "2382") THEN DO:
/*[EXECUTABLE]*/                           IF trim(EntS7072.PolicyTypeCd) <> "" THEN DO:  /*»ÃÐàÀ· 1 2 3*/
/*[EXECUTABLE]*/                               FIND LAST IntPol7072 USE-INDEX IntPol707210 WHERE
/*[EXECUTABLE]*/                                   IntPol7072.CompanyCode               = EntS7072.CompanyCode AND 
/*[EXECUTABLE]*/                                  ( substr(IntPol7072.PolicyTypeCd,1,1) = "1"   OR 
/*[EXECUTABLE]*/                                   substr(IntPol7072.PolicyTypeCd,1,1)  = "2"   OR
/*[EXECUTABLE]*/                                   substr(IntPol7072.PolicyTypeCd,1,1)  = "3" ) AND
/*[EXECUTABLE]*/                                   IntPol7072.ChassisSerialNumber       = EntS7072.ChassisSerialNumber  
/*[EXECUTABLE]*/                                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                               IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                                   IF  EntS7072.EffectiveDt  < IntPol7072.ExpirationDt THEN DO:
/*[EXECUTABLE]*/                                       IF INDEX(IntPol7072.ContractNumber,"CA") <> 0 THEN
/*[EXECUTABLE]*/                                       nv_msgerrordupl = "¾ºàÅ¢·ÕèµÑÇ¶Ñ§«éÓ : " + EntS7072.ChassisSerialNumber + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                               END.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           ELSE DO: /*compulsary*/
/*[EXECUTABLE]*/                               FIND LAST IntPol7072 USE-INDEX IntPol707211 WHERE
/*[EXECUTABLE]*/                                   IntPol7072.CompanyCode         = EntS7072.CompanyCode AND 
/*[EXECUTABLE]*/                                   IntPol7072.CMIPolicyTypeCd     = TRIM(EntS7072.CMIPolicyTypeCd) AND
/*[EXECUTABLE]*/                                   IntPol7072.ChassisSerialNumber = EntS7072.ChassisSerialNumber   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                               IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                                   IF  EntS7072.CMIEffectiveDt  < IntPol7072.CMIExpirationDt THEN DO:
/*[EXECUTABLE]*/                                       IF INDEX(IntPol7072.ContractNumber,"CA") <> 0 THEN 
/*[EXECUTABLE]*/                                       nv_msgerrordupl = "¾ºàÅ¢·ÕèµÑÇ¶Ñ§«éÓ : " + EntS7072.ChassisSerialNumber + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                               END.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                       END. /*end. 570 */
/*[EXECUTABLE]*/                       IF  EntS7072.PolicyStatus = "N"  THEN DO:
/*[EXECUTABLE]*/                           DEF VAR nv_titleNM AS CHAR FORMAT "x(150)".
/*[EXECUTABLE]*/                           ASSIGN nv_titleNM = ""
/*[EXECUTABLE]*/                               nv_titleNM = EntS7072.InsuredTitle + EntS7072.InsuredName +  EntS7072.InsuredSurname.
/*[EXECUTABLE]*/                           IF EntS7072.PolicyTypeCd = "2.1" OR EntS7072.PolicyTypeCd = "2.2"  OR
/*[EXECUTABLE]*/                               EntS7072.PolicyTypeCd = "3.1" OR EntS7072.PolicyTypeCd = "3.2" OR EntS7072.PolicyTypeCd = "3" THEN DO:
/*[EXECUTABLE]*/                               IF  R-INDEX(TRIM(nv_titleNM),"¨¡.")             <> 0  OR R-INDEX(TRIM(nv_titleNM),"¨Ó¡Ñ´")           <> 0  OR  
/*[EXECUTABLE]*/                                   R-INDEX(TRIM(nv_titleNM),"(ÁËÒª¹)")         <> 0  OR R-INDEX(TRIM(nv_titleNM),"INC.")            <> 0  OR 
/*[EXECUTABLE]*/                                   R-INDEX(TRIM(nv_titleNM),"CO.")             <> 0  OR R-INDEX(TRIM(nv_titleNM),"LTD.")            <> 0  OR 
/*[EXECUTABLE]*/                                   R-INDEX(TRIM(nv_titleNM),"LIMITED")         <> 0  OR INDEX(TRIM(nv_titleNM),"ºÃÔÉÑ·")            <> 0  OR 
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"º.")                <> 0  OR INDEX(TRIM(nv_titleNM),"º¨¡.")              <> 0  OR 
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"Ë¨¡.")              <> 0  OR INDEX(TRIM(nv_titleNM),"ËÊ¹.")              <> 0  OR 
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"ºÃÃÉÑ·")            <> 0  OR INDEX(TRIM(nv_titleNM),"ÁÙÅ¹Ô¸Ô")           <> 0  OR 
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"ËéÒ§")              <> 0  OR INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹")      <> 0  OR 
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹¨Ó¡Ñ´") <> 0  OR INDEX(TRIM(nv_titleNM),"ËéÒ§ËØé¹ÊèÇ¹¨Ó¡")   <> 0  OR  
/*[EXECUTABLE]*/                                   INDEX(TRIM(nv_titleNM),"áÅÐ/ËÃ×Í")          <> 0  THEN 
/*[EXECUTABLE]*/                                   nv_msgerrordupl = "¾º§Ò¹¹ÔµÔºØ¤Å: " + EntS7072.ContractNumber + "ª×èÍ : " + 
/*[EXECUTABLE]*/                                   EntS7072.InsuredTitle + " " + EntS7072.InsuredName + " " +  EntS7072.InsuredSurname.   
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* Add Kridtiya i. Date.11/01/2018 à¾ÔèÁ ¡ÒÃàªç¤ ¹ÔµÔºØ¤¤Å*/
/*[EXECUTABLE]*/                       IF EntS7072.MsgStatusCd <> "TEST" THEN DO:
/*[EXECUTABLE]*/                         IF EntS7072.CompanyCode = "M82" THEN DO:
/*[EXECUTABLE]*/                             DEF VAR nv_contactM82 AS CHAR INIT "".
/*[EXECUTABLE]*/                             ASSIGN nv_contactM82 =  substr(EntS7072.ContractNumber,1,11).
/*[COMMENT]*/                                /*FIND FIRST IntPol7072 USE-INDEX IntPol707205 WHERE*/
/*[EXECUTABLE]*/                             RUN Pc_CheckMsg (INPUT ("in check contract" + EntS7072.ContractNumber )).
/*[EXECUTABLE]*/                             FIND LAST IntPol7072  USE-INDEX  IntPol707212 WHERE
/*[EXECUTABLE]*/                                 IntPol7072.CompanyCode    = EntS7072.CompanyCode AND   
/*[EXECUTABLE]*/                                 IntPol7072.SERVICE_RUN_NO = nv_contactM82    NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                             IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                                 IF INDEX(IntPol7072.ContractNumber,"CA") = 0 THEN DO:
/*[EXECUTABLE]*/                                   IF IntPol7072.PolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                     nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                     + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
/*[EXECUTABLE]*/                                   ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                     nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                     + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
/*[EXECUTABLE]*/                                 END.
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             RUN Pc_CheckMsg (INPUT "out check contract" ).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         ELSE DO:
/*[EXECUTABLE]*/                           FIND FIRST IntPol7072 USE-INDEX IntPol707205 WHERE
/*[EXECUTABLE]*/                             IntPol7072.ContractNumber = EntS7072.ContractNumber AND
/*[EXECUTABLE]*/                             IntPol7072.CompanyCode    = EntS7072.CompanyCode
/*[EXECUTABLE]*/                             NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                           IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                               IF IntPol7072.CompanyCode = "1018" THEN DO:
/*[EXECUTABLE]*/                                   ASSIGN nv_RecIntPol7072 = RECID(IntPol7072).
/*[EXECUTABLE]*/                                   IF  IntPol7072.AttachFile1  = ? THEN DO:
/*[EXECUTABLE]*/                                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072 NO-ERROR.
/*[EXECUTABLE]*/                                       IF AVAILABLE IntPol7072 THEN  
/*[EXECUTABLE]*/                                           ASSIGN IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA".
/*[EXECUTABLE]*/                                       RELEASE IntPol7072.
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                                   ELSE DO:
/*[EXECUTABLE]*/                                       IF IntPol7072.PolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                           nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                           + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
/*[EXECUTABLE]*/                                       ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                           nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                           + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                               END.
/*[EXECUTABLE]*/                               ELSE DO:
/*[EXECUTABLE]*/                                   IF IntPol7072.PolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                       nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                       + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.PolicyNumber.
/*[EXECUTABLE]*/                                   ELSE IF IntPol7072.CMIPolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                                       nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                       + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntPol7072.CMIPolicyNumber.
/*[EXECUTABLE]*/                               END.  /* case no 1018*/
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END. /*ELSE DO:*/
/*[EXECUTABLE]*/                         IF nv_msgerrordupl  = "" THEN DO:
/*[EXECUTABLE]*/                           FIND FIRST IntS7072 WHERE
/*[EXECUTABLE]*/                                      IntS7072.CompanyCode    = EntS7072.CompanyCode
/*[EXECUTABLE]*/                                AND   IntS7072.ContractNumber = EntS7072.ContractNumber
/*[EXECUTABLE]*/                           NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                           IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                             IF IntS7072.PolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                               nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                               + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntS7072.PolicyNumber.
/*[EXECUTABLE]*/                             ELSE IF IntS7072.CMIPolicyNumber <> "" THEN 
/*[EXECUTABLE]*/                               nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber
/*[EXECUTABLE]*/                                               + " ·Õè¡ÃÁ¸ÃÃÁìàÅ¢·Õè: " + IntS7072.CMIPolicyNumber.
/*[EXECUTABLE]*/                             ELSE nv_msgerrordupl = "¾ºàÅ¢·ÕèÍéÒ§ÍÔ§: " + EntS7072.ContractNumber + " «éÓã¹ÃÐºº".
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           RUN Pc_CheckMsg (INPUT "in check contract/IntS7072" ).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF EntS7072.MsgStatusCd <> "TEST" THEN DO:*/
/*[EXECUTABLE]*/                     END. /*IF nv_msgerrordupl = "" THEN DO:*/
/*[EXECUTABLE]*/                     IF nv_msgerrordupl = "" THEN DO:
/*[EXECUTABLE]*/                       IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO: /* 3.1, 3.2, 3*/
/*[COMMENT]*/                                                               /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                         RUN WSP/WSPMCpny.P (EntS7072.CompanyCode
/*[EXECUTABLE]*/                                            ,EntS7072.BranchCd
/*[EXECUTABLE]*/                                            ,"V70"
/*[EXECUTABLE]*/                                            ,OUTPUT nv_BrokerCompany
/*[EXECUTABLE]*/                                            ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                                            ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                                            ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                                            ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                         IF EntS7072.DocumentUID <> ""  THEN DO:
/*[EXECUTABLE]*/                           ASSIGN
/*[EXECUTABLE]*/                           nv_Mdocno1  = EntS7072.DocumentUID
/*[EXECUTABLE]*/                           nv_Mtrty11  = "M".
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF TRIM(EntS7072.PolicyTypeCd) <> "" AND EntS7072.RateGroup <> "" THEN DO:*/
/*[EXECUTABLE]*/                       IF nv_msgerrordupl = "" THEN DO:
/*[EXECUTABLE]*/                         IF TRIM(EntS7072.CMIPolicyTypeCd) <> "" AND EntS7072.CMIVehTypeCd <> "" THEN DO: /* ¾Ãº. */
/*[EXECUTABLE]*/                           RUN WSP/WSPMCpny.P (EntS7072.CompanyCode  /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                                              ,EntS7072.BranchCd
/*[EXECUTABLE]*/                                              ,"V72"
/*[EXECUTABLE]*/                                              ,OUTPUT nv_BrokerCompany
/*[EXECUTABLE]*/                                              ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                                              ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                                              ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                                              ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                           IF EntS7072.CMIDocumentUID <> "" THEN DO:
/*[EXECUTABLE]*/                             ASSIGN nv_Tdocno1  = EntS7072.CMIDocumentUID
/*[EXECUTABLE]*/                             nv_Ttrty11  = "T".
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_msgerrordupl = "" THEN DO:
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = EntS7072.CMIPolicyTypeCd.
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,CHR(13),"").
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd," ","").
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,".","").
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,"-","").
/*[EXECUTABLE]*/                             nv_PolicyTypeCd = REPLACE(nv_PolicyTypeCd,"+","").
/*[EXECUTABLE]*/                             FOR EACH msgcode WHERE MsgCode.CompNo = EntS7072.CompanyCode
/*[EXECUTABLE]*/                                  AND MsgCode.MsgNo  = "GrpClass"
/*[EXECUTABLE]*/                             NO-LOCK:
/*[EXECUTABLE]*/                               IF msgcode.Branch = nv_PolicyTypeCd AND MsgCode.MsgDesc = "NOTSALE" THEN
/*[EXECUTABLE]*/                                 nv_msgerrordupl = "äÁè¾ºÃËÑÊ¾Ãº. " + EntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ".
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     RUN Pc_CheckMsg (INPUT "in check contract/PD_ChkDataExt1" ).
/*[EXECUTABLE]*/                     IF nv_msgerrordupl = "" THEN DO:
/*[EXECUTABLE]*/                       FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
/*[EXECUTABLE]*/                       NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                       IF AVAILABLE EntS7072 THEN DO:
/*[EXECUTABLE]*/                         RUN PD_ChkDataExt1 (INPUT nv_Mdocno1
/*[EXECUTABLE]*/                                            ,INPUT nv_Tdocno1).
/*[EXECUTABLE]*/                         nv_process       = "Data".
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END. /* FIND FIRST  EntS7072  WHERE*/
/*[EXECUTABLE]*/                 IF nv_msgerrordupl = "" THEN DO:
/*[EXECUTABLE]*/                   IF nv_RECIDEntS7072 <> 0 AND nv_RECIDEntS7072 <> ? THEN DO:
/*[EXECUTABLE]*/                     OUTPUT STREAM xmlstream TO PUT_EntS7072.TXT  APPEND .
/*[EXECUTABLE]*/                     FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE EntS7072 THEN DO:
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                       " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "MethodCode SPLIT   |" EntS7072.MethodCode       SKIP.  
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "1  Username        |" EntS7072.Username         SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "2  Password        |" EntS7072.Password         SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "3  CompanyCode     |" EntS7072.CompanyCode      SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "3  BranchCd        |" EntS7072.BranchCd         SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "7  ContractNumber  |" EntS7072.ContractNumber   FORMAT "X(30)" SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "4  EnsurerId       |" EntS7072.InsurerId        SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "4  InsuranceCd     |" EntS7072.InsuranceCd      SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "5  PolicyNumber    |" EntS7072.PolicyNumber     SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "7  nv_DocnoV70     |" EntS7072.DocumentUID      FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "24 InsuredName     |" EntS7072.InsuredName      SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "25 InsuredSurname  |" EntS7072.InsuredSurname   SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "30 PolicyTypeCd    |" EntS7072.PolicyTypeCd     SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "31 RateGroup       |" EntS7072.RateGroup        SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "27 WrittenAmt      |" EntS7072.WrittenAmt       SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "5  CMIPolicyNumber |" EntS7072.CMIPolicyNumber  FORMAT "x(20)" SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "7  CMIDocumentUID  |" EntS7072.CMIDocumentUID   FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "7  CMIBarCodeNumber|" EntS7072.CMIBarCodeNumber FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "32 CMIPolicyTypeCd |" EntS7072.CMIPolicyTypeCd  SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "33 CMIVehTypeCd    |" EntS7072.CMIVehTypeCd     SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "34 CMIWrittenAmt   |" EntS7072.CMIWrittenAmt    SKIP.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "35 SERVICE_ID      |" EntS7072.SERVICE_ID  SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream  FILL("-",78) FORMAT "X(78)" SKIP(1).
/*[EXECUTABLE]*/                       PUT STREAM xmlstream  "/***************************************************/" SKIP(1).
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     OUTPUT STREAM xmlstream CLOSE.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END. /*IF nv_msgerrordupl = "" THEN DO:*/
/*[EXECUTABLE]*/                 ELSE DO:
/*[EXECUTABLE]*/                   IF nv_RECIDEntS7072 <> 0 AND nv_RECIDEntS7072 <> ? THEN DO:
/*[EXECUTABLE]*/                     RUN Pc_SAVEDUPL (nv_RECIDEntS7072, nv_DuplPolicy, nv_msgerrordupl).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END. /*IF nv_msgerrordupl = "" THEN DO:*/
/*[EXECUTABLE]*/                 nv_LongCount = 0.
/*[EXECUTABLE]*/                 RELEASE EntS7072Result.
/*[EXECUTABLE]*/                 RELEASE IntS7072.
/*[EXECUTABLE]*/                 RELEASE EntS7072.
/*[EXECUTABLE]*/                 RELEASE IntPol7072.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_CheckMsg C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_CheckMsg :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_msgtext   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 OUTPUT STREAM xmlstream TO PUT_EntS7072.TXT  APPEND .
/*[BLANK]*/                        
/*[EXECUTABLE]*/                     PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                       " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                       PUT STREAM xmlstream "Text   |" nv_msgtext      SKIP.   
/*[EXECUTABLE]*/                       PUT STREAM xmlstream  "/***************************************************/" SKIP(1).
/*[BLANK]*/                          
/*[EXECUTABLE]*/                     OUTPUT STREAM xmlstream CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVECKAGT C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVECKAGT :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT         PARAMETER  nv_BrokerCompany  AS CHAR FORMAT "x(20)".
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT  PARAMETER  nv_chkerror       AS CHAR FORMAT "x(500)".  
/*[COMMENT]*/                    /*DEF VAR nv_BrokerCompany AS CHAR FORMAT "x(12)" INIT "". */
/*[EXECUTABLE]*/                 DEF VAR nv_agent         AS CHAR FORMAT "x(20)" INIT "".
/*[EXECUTABLE]*/                 DEF VAR nv_producer      AS CHAR FORMAT "x(20)" INIT "" .
/*[COMMENT]*/                    /*DEF VAR nv_chkerror      AS CHAR FORMAT "x(500)" . */
/*[EXECUTABLE]*/                 DEF BUFFER bxmm600       FOR  sic_bran.xmm600.
/*[BLANK]*/                      
/*[BLANK]*/                           
/*[EXECUTABLE]*/                 FIND LAST  stat.company USE-INDEX Company01 WHERE
/*[EXECUTABLE]*/                     stat.company.Compno = nv_BrokerCompany  /*ºÃÔÉÑ·ã¹ÃÐºº*/
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAIL stat.company THEN 
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     nv_agent         = stat.company.Agent
/*[EXECUTABLE]*/                     nv_producer      = stat.company.Acno1.
/*[EXECUTABLE]*/                 ELSE DO:
/*[EXECUTABLE]*/                         OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                         PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                             " äÁè¾ºÃËÑÊºÃÔÉÑ·: "  nv_BrokerCompany   FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 FIND LAST sic_bran.xmm600 WHERE xmm600.acno = TRIM(nv_agent) NO-LOCK NO-ERROR.
/*[EXECUTABLE]*/                 IF AVAIL sic_bran.xmm600 THEN DO:
/*[COMMENT]*/                        /* àªç¤ lincen Agent code */
/*[EXECUTABLE]*/                     IF sic_bran.xmm600.agtreg = "" THEN DO:  /* agent äÁèÁÕ licen */
/*[EXECUTABLE]*/                         ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
/*[EXECUTABLE]*/                         OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                         PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                             nv_agent  " Agent Licence no. is Null "  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN DO:  
/*[COMMENT]*/                            /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
/*[EXECUTABLE]*/                         ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
/*[EXECUTABLE]*/                         OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                         PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                             nv_agent  " Agent Licence Expire Date: " STRING(sic_bran.xmm600.regdate,"99/99/9999") FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE IF sic_bran.xmm600.iblack <> "" THEN DO:  /* check over credit */
/*[EXECUTABLE]*/                         ASSIGN nv_chkerror = nv_chkerror + "| Agent code has OVER CREDIT " .
/*[EXECUTABLE]*/                         OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                         PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                             nv_agent  " Agent code has OVER CREDIT"  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
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
/*[EXECUTABLE]*/                     ELSE DO: /* Agent code Client type <> DI ,Check Producer */
/*[EXECUTABLE]*/                         FIND LAST bxmm600 WHERE bxmm600.acno = TRIM(nv_producer) NO-LOCK NO-ERROR.
/*[EXECUTABLE]*/                         IF AVAIL bxmm600 THEN DO:
/*[COMMENT]*/                                /* àªç¤ÇÑ¹·Õè»Ô´â¤é´ ¹éÍÂ¡ÇèÒ ÇÑ¹·Õè¤ØéÁ¤ÃÍ§ */
/*[EXECUTABLE]*/                             IF bxmm600.closed <> ? AND bxmm600.closed < TODAY THEN DO: 
/*[EXECUTABLE]*/                                 ASSIGN nv_chkerror = nv_chkerror + "| Producer Code Closed Date: " + STRING(bxmm600.closed,"99/99/9999") .
/*[EXECUTABLE]*/                                 OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                                 PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                     nv_producer  " Producer Code Closed Date: " STRING(bxmm600.closed,"99/99/9999") FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                                 OUTPUT CLOSE.
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             ELSE DO: 
/*[COMMENT]*/                                    /* ÂÑ§äÁè»Ô´â¤é´ àªç¤ Over Credit */
/*[EXECUTABLE]*/                                 IF bxmm600.iblack <> "" THEN DO: 
/*[EXECUTABLE]*/                                     ASSIGN nv_chkerror = nv_chkerror + "| Producer code has OVER CREDIT " .
/*[EXECUTABLE]*/                                     OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                                     PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                         nv_producer  " Producer code has OVER CREDIT"  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                                 END.
/*[EXECUTABLE]*/                                 ELSE DO: 
/*[COMMENT]*/                                        /* äÁèµÔ´ Over credit */
/*[EXECUTABLE]*/                                     IF bxmm600.agtreg = "" THEN DO: /* producer code äÁèÁÕ licen */
/*[COMMENT]*/                                            /* àªç¤ lincen Agent code */
/*[EXECUTABLE]*/                                         IF sic_bran.xmm600.agtreg = "" THEN DO:  /* agent äÁèÁÕ licen */
/*[EXECUTABLE]*/                                             ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence no. is Null " .
/*[EXECUTABLE]*/                                             OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                                             PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                                 nv_producer  " Agent Licence no. is Null"  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                                             OUTPUT CLOSE.
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                         ELSE DO: /* agent ÁÕ Lincen àªç¤ ÇÑ¹·Õè licen expire */
/*[EXECUTABLE]*/                                             IF sic_bran.xmm600.regdate <> ? AND sic_bran.xmm600.regdate < TODAY THEN DO:
/*[EXECUTABLE]*/                                                 ASSIGN nv_chkerror = nv_chkerror + "| Agent Licence Expire Date: " + STRING(sic_bran.xmm600.regdate,"99/99/9999") .
/*[EXECUTABLE]*/                                                 OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                                                 PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                                     nv_producer  " Agent Licence Expire Date: "   STRING(sic_bran.xmm600.regdate,"99/99/9999")  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                                                 OUTPUT CLOSE.
/*[EXECUTABLE]*/                                             END.
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                     ELSE DO: /* producer code ÁÕ licen check licen expire */
/*[EXECUTABLE]*/                                         IF bxmm600.regdate <> ? AND bxmm600.regdate < TODAY THEN DO:
/*[EXECUTABLE]*/                                             ASSIGN nv_chkerror = nv_chkerror + "| Producer Licence Expire Date: " + STRING(bxmm600.regdate,"99/99/9999") .
/*[EXECUTABLE]*/                                             OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                                             PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                                 nv_producer " Producer Licence Expire Date: "   STRING(bxmm600.regdate,"99/99/9999")  FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                                             OUTPUT CLOSE.
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                 END.
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         ELSE DO: 
/*[EXECUTABLE]*/                             ASSIGN nv_chkerror = nv_chkerror + "| Not found Producer Code " + nv_producer  + " ·Õèxmm600 ".
/*[EXECUTABLE]*/                             OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                             PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                                 " Not found Producer Code "  nv_producer    FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                             OUTPUT CLOSE.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO: 
/*[EXECUTABLE]*/                     ASSIGN nv_chkerror = nv_chkerror + "| Not found Agent Code " + nv_agent + " ·Õèxmm600 ".
/*[EXECUTABLE]*/                     OUTPUT TO Log_CheckAgentLimit.TXT APPEND.
/*[EXECUTABLE]*/                     PUT   TODAY FORMAT "99/99/9999" " Time: " STRING(TIME,"HH:MM:SS")  
/*[EXECUTABLE]*/                         " Not found Agent Code "   nv_agent    FORMAT "x(100)" SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                       
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVECKGrp C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVECKGrp :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_racidck  AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_msgerrorgrp   AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND EntS7072 WHERE RECID(EntS7072) = nv_racidck
/*[EXECUTABLE]*/                 NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE EntS7072 THEN DO:
/*[COMMENT]*/                        /*§Ò¹·ÕèäÁèµéÍ§µÃÇ¨ÊÀÒ¾ ÅçÍ¤·Ñé§ËÁ´¤èÐ Â¡àÇé¹¾Ãº.áÅÐ»ÃÐàÀ· 3  ÍéÒ§ÍÔ§¨Ò¡¾ÃÕàÁÕèÂÁä´éàÅÂ ·Ø¡ÃØè¹¤èÐ àËÁ×Í¹¡ÅØèÁ 1 
/*[COMMENT]*/                                 2.2 áÅÐ 3.2  */
/*[EXECUTABLE]*/                     IF   trim(EntS7072.Manufacturer) = "PROTON" THEN DO:
/*[EXECUTABLE]*/                         IF EntS7072.CMIPolicyTypeCd = ""  THEN  /*V72 not check */
/*[EXECUTABLE]*/                             nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model + "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE IF ((substr(EntS7072.PolicyTypeCd,1,1)  = "2" ) AND  (  deci(EntS7072.COLLAmtAccident) < 350001 )  ) OR 
/*[EXECUTABLE]*/                         (EntS7072.PolicyTypeCd = "3.1")  OR (EntS7072.PolicyTypeCd = "3.2") THEN DO:
/*[BLANK]*/                              
/*[EXECUTABLE]*/                         FIND LAST FVehGrp USE-INDEX FVehGrp01 WHERE 
/*[EXECUTABLE]*/                             FVehGrp.MakDes  = EntS7072.Manufacturer  AND     
/*[EXECUTABLE]*/                             FVehGrp.ModDes  = EntS7072.Model         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAIL FVehGrp  THEN DO:
/*[EXECUTABLE]*/                             IF FVehGrp.Vehgrp  = "1" OR FVehGrp.Vehgrp = "2" THEN
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ " + FVehGrp.Vehgrp + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[BLANK]*/                                  
/*[EXECUTABLE]*/                         END.
/*[COMMENT]*/                            /*ELSE nv_msgerrorgrp  = "".*/
/*[EXECUTABLE]*/                         IF nv_msgerrorgrp  = ""  THEN DO:
/*[BLANK]*/                               
/*[EXECUTABLE]*/                             IF   EntS7072.Manufacturer = "TOYOTA"  AND 
/*[EXECUTABLE]*/                                 (EntS7072.Model = "BB"       OR EntS7072.Model = "Estima"       OR
/*[EXECUTABLE]*/                                  EntS7072.Model = "Harrier"  OR EntS7072.Model = "Land Cruiser" OR 
/*[EXECUTABLE]*/                                  EntS7072.Model = "Vellfire" OR EntS7072.Model = "Alphard"      OR
/*[EXECUTABLE]*/                                  EntS7072.Model = "Voxy"     OR EntS7072.Model = "Crown"        OR
/*[EXECUTABLE]*/                                  EntS7072.Model = "Vitz"     OR EntS7072.Model = "ALtezza"      OR EntS7072.Model = "COMMUTER"  ) THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF EntS7072.Manufacturer = "Chevrolet" AND
/*[EXECUTABLE]*/                                 (EntS7072.Model = "Camero" OR EntS7072.Model = "Express") THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF EntS7072.Manufacturer = "Nissan" AND
/*[EXECUTABLE]*/                                 (EntS7072.Model = "Cube"     OR EntS7072.Model = "Fairlady" OR EntS7072.Model = "GT-R") THEN
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF EntS7072.Manufacturer = "Honda"  AND EntS7072.Model = "S660"    THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF EntS7072.Manufacturer = "Suzuki" AND EntS7072.Model = "Lapin"   THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF EntS7072.Manufacturer = "Ford"   AND EntS7072.Model = "Tunland" THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE IF index(EntS7072.Manufacturer,"BENZ") <> 0  THEN 
/*[EXECUTABLE]*/                                 nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model +
/*[EXECUTABLE]*/                                                   " ¡ÅØèÁ Grey Market" + "äÁèÃÑº»ÃÐ¡Ñ¹ÀÑÂ»ÃÐàÀ· " + EntS7072.PolicyTypeCd +
/*[EXECUTABLE]*/                                                   "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹".
/*[EXECUTABLE]*/                             ELSE nv_msgerrorgrp  = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE nv_msgerrorgrp  = "".
/*[EXECUTABLE]*/                     IF EntS7072.Manufacturer = "Chevrolet" THEN DO:
/*[EXECUTABLE]*/                         IF (trim(EntS7072.PolicyTypeCd)  = "1"   OR trim(EntS7072.PolicyTypeCd)  = "2.1" OR trim(EntS7072.PolicyTypeCd)  = "2.2" OR
/*[EXECUTABLE]*/                             trim(EntS7072.PolicyTypeCd)  = "3.1" OR trim(EntS7072.PolicyTypeCd)  = "3.2") AND 
/*[EXECUTABLE]*/                             ((YEAR(today) - deci(EntS7072.ModelYear)) + 1 > 5 ) THEN
/*[EXECUTABLE]*/                             nv_msgerrorgrp = "¾ºÃ¶ " + EntS7072.Manufacturer + "/" + EntS7072.Model + "¡ÃØ³ÒµÔ´µèÍ½èÒÂÃÑº»ÃÐ¡Ñ¹ ÍÒÂØÃ¶à¡Ô¹ 5 »Õ".
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 IF nv_msgerrorgrp <> "" THEN DO:
/*[EXECUTABLE]*/                     OUTPUT TO PC_SaveERRGroup12.txt APPEND.
/*[EXECUTABLE]*/                     PUT "-------------------------" SKIP
/*[EXECUTABLE]*/                         " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP
/*[EXECUTABLE]*/                         "Error : " nv_msgerrorgrp  FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEDUPL C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVEDUPL :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDEntS7072  AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_DuplPolicy     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_msgerrordupl   AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_DuplContract          AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_DuplPolicy <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_DuplPolicy /*70*/
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE IntPol7072 THEN
/*[EXECUTABLE]*/                      nv_DuplContract = IntPol7072.ContractNumber.
/*[EXECUTABLE]*/                   ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_DuplPolicy /*72*/
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE IntPol7072 THEN
/*[EXECUTABLE]*/                        nv_DuplContract = IntPol7072.ContractNumber.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE EntS7072 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST EntS7072Result WHERE
/*[EXECUTABLE]*/                              EntS7072Result.SystemRq        = EntS7072.SystemRq
/*[EXECUTABLE]*/                          AND EntS7072Result.CompanyCode     = EntS7072.CompanyCode
/*[EXECUTABLE]*/                          AND EntS7072Result.ContractNumber  = EntS7072.ContractNumber
/*[EXECUTABLE]*/                          AND EntS7072Result.keyRequestIndRq = EntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                   NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF NOT AVAILABLE EntS7072Result THEN  CREATE EntS7072Result.
/*[BLANK]*/                       
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                   EntS7072Result.CompanyCode          = EntS7072.CompanyCode
/*[EXECUTABLE]*/                   EntS7072Result.ReferenceNumber      = EntS7072.ReferenceNumber
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.InsurerCode          = EntS7072.InsurerCode
/*[EXECUTABLE]*/                   EntS7072Result.MethodCode           = EntS7072.MethodCode
/*[EXECUTABLE]*/                   EntS7072Result.Policy               = EntS7072.Policy
/*[EXECUTABLE]*/                   EntS7072Result.Rencnt               = EntS7072.Rencnt
/*[EXECUTABLE]*/                   EntS7072Result.Endcnt               = EntS7072.Endcnt
/*[EXECUTABLE]*/                   EntS7072Result.Riskno               = EntS7072.Riskno
/*[EXECUTABLE]*/                   EntS7072Result.Itemno               = EntS7072.Itemno
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.ProcessByUser        = EntS7072.ProcessByUser
/*[EXECUTABLE]*/                   EntS7072Result.ProcessDate          = EntS7072.ProcessDate
/*[EXECUTABLE]*/                   EntS7072Result.ProcessTime          = EntS7072.ProcessTime
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.TrnFromIntDate       = EntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                   EntS7072Result.TrnFromIntTime       = EntS7072.TrnFromIntTime
/*[EXECUTABLE]*/                   EntS7072Result.ReceiveNumber        = EntS7072.ReceiveNumber
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.SystemRq             = EntS7072.SystemRq
/*[EXECUTABLE]*/                   EntS7072Result.InsurerId            = EntS7072.InsurerId
/*[EXECUTABLE]*/                   EntS7072Result.ContractNumber       = EntS7072.ContractNumber
/*[EXECUTABLE]*/                   EntS7072Result.RqUID                = EntS7072.RqUID
/*[EXECUTABLE]*/                   EntS7072Result.keyRequestIndRq      = EntS7072.keyRequestIndRq
/*[BLANK]*/                       
/*[EXECUTABLE]*/                   EntS7072Result.RecordGUIDRs         = ""
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.TransactionResponseDt   =   STRING( YEAR(EntS7072.TrnFromIntDate),"9999")
/*[EXECUTABLE]*/                                                            + STRING(MONTH(EntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                                                            + STRING(  DAY(EntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                   EntS7072Result.TransactionResponseTime = EntS7072.TrnFromIntTime
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.PolicyNumber         = EntS7072.PolicyNumber
/*[EXECUTABLE]*/                   EntS7072Result.DocumentUID          = EntS7072.DocumentUID
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.CMIPolicyNumber      = EntS7072.CMIPolicyNumber
/*[EXECUTABLE]*/                   EntS7072Result.CMIDocumentUID       = EntS7072.CMIDocumentUID
/*[EXECUTABLE]*/                   EntS7072Result.MsgStatusCd          = "FAIL"
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      EntS7072Result.CMIBarCodeNumber     = EntS7072.CMIBarCodeNumber
/*[COMMENT]*/                      */
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   EntS7072Result.vehreg               = EntS7072.vehreg
/*[EXECUTABLE]*/                   EntS7072Result.Adjustno             = 0
/*[COMMENT]*/                      /* */
/*[EXECUTABLE]*/                   EntS7072Result.ResultStatus         = "FAIL"                 /*EntS7072.ResultStatus*/
/*[EXECUTABLE]*/                   EntS7072Result.ErrorCode            = ""
/*[EXECUTABLE]*/                   EntS7072Result.ErrorMessage         = nv_msgerrordupl
/*[COMMENT]*/                      /* */
/*[EXECUTABLE]*/                   EntS7072Result.LinkStatus           = "E"      .
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_DuplPolicy <> "" AND nv_DuplContract <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     EntS7072Result.EndorseRefNumber     = nv_DuplPolicy  .
/*[EXECUTABLE]*/                     EntS7072Result.EndorseReceiveNumber = nv_DuplContract.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF EntS7072.InsurerId = "" THEN EntS7072Result.InsurerId = EntS7072.CompanyCode.
/*[BLANK]*/                         
/*[EXECUTABLE]*/                   EntS7072.ProcessStatus = "X" .
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ----------------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RELEASE EntS7072Result.
/*[EXECUTABLE]*/                 RELEASE EntS7072.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEFMRate C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVEFMRate :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_RECIDEntS7072  AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_msgerrorgrp    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VAR    nv_sumins  AS DECI INIT  0.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_msgerrorgrp = "" THEN DO:
/*[BLANK]*/                          
/*[EXECUTABLE]*/                 FIND EntS7072 WHERE RECID(EntS7072) = nv_RECIDEntS7072
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE EntS7072 THEN DO:
/*[EXECUTABLE]*/                     IF EntS7072.RateGroup <> "" THEN DO:
/*[EXECUTABLE]*/                         IF EntS7072.RateGroup  <> "110"  AND EntS7072.RateGroup  <> "210" AND EntS7072.RateGroup  <> "320" AND EntS7072.RateGroup  <> "610" THEN 
/*[EXECUTABLE]*/                             nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
/*[EXECUTABLE]*/                         ELSE DO:
/*[EXECUTABLE]*/                             IF      EntS7072.RateGroup  = "320" THEN DO:
/*[EXECUTABLE]*/                                 IF  deci(EntS7072.GrossVehOrCombinedWeight) > 4 THEN
/*[EXECUTABLE]*/                                     nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¹éÓË¹Ñ¡Ã¶¤×Í " +   EntS7072.GrossVehOrCombinedWeight + "à¡Ô¹ 4 µÑ¹" .
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             ELSE IF EntS7072.RateGroup  = "210" THEN DO: 
/*[EXECUTABLE]*/                                 IF INTE(EntS7072.SeatingCapacity) > 12 THEN
/*[EXECUTABLE]*/                                     nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¨Ó¹Ç¹·Õè¹Ñè§" +   EntS7072.SeatingCapacity + "à¡Ô¹ 12 ·Õè¹Ñè§" .
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             ELSE IF EntS7072.RateGroup  = "110" THEN DO: 
/*[EXECUTABLE]*/                                 IF INTE(EntS7072.SeatingCapacity) > 7 THEN
/*[EXECUTABLE]*/                                     nv_msgerrorgrp = "¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " ¨Ó¹Ç¹·Õè¹Ñè§" +   EntS7072.SeatingCapacity + "à¡Ô¹ 7 ·Õè¹Ñè§" .
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                             ELSE nv_msgerrorgrp = "äÁè¾ºÃËÑÊÃ¶ : " + EntS7072.RateGroup + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF nv_msgerrorgrp = "" THEN DO:
/*[EXECUTABLE]*/                         IF EntS7072.Model = "commuter" THEN DO:
/*[EXECUTABLE]*/                             IF substr(TRIM(EntS7072.Registration),1,1) >= "0" AND  substr(TRIM(EntS7072.Registration),1,1) <= "9" THEN DO:
/*[EXECUTABLE]*/                                 IF substr(TRIM(EntS7072.Registration),2,1) >= "0" AND  substr(TRIM(EntS7072.Registration),2,1) <= "9" THEN 
/*[EXECUTABLE]*/                                     nv_msgerrorgrp = "¾º·ÐàºÕÂ¹Ã¶à»ç¹µÑÇàÅ¢ : " + EntS7072.Registration + " !!! ¡ÃØ³ÒÊè§ãºá¨é§§Ò¹ãËéà¨éÒË¹éÒ·Õè¡ÒÃµÅÒ´¾Ô¨ÒÃ³Ò "  .
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF nv_msgerrorgrp = "" THEN DO:
/*[EXECUTABLE]*/                         IF EntS7072.PolicyStatus = "N" /*AND EntS7072.CompanyCode <> "834"*/   THEN DO:
/*[EXECUTABLE]*/                             nv_sumins = deci(EntS7072.COLLAmtAccident).
/*[EXECUTABLE]*/                             IF  EntS7072.RateGroup  <> "" AND EntS7072.PolicyTypeCd <> "" AND EntS7072.PolicyTypeCd <> "1" THEN DO:
/*[EXECUTABLE]*/                                 IF EntS7072.PolicyTypeCd = "3" THEN DO:
/*[EXECUTABLE]*/                                     IF EntS7072.CurrentTermAmt <> ""  THEN DO:
/*[EXECUTABLE]*/                                         FIND FIRST FMRate WHERE
/*[COMMENT]*/                                                /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
/*[EXECUTABLE]*/                                             FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/ 
/*[EXECUTABLE]*/                                             FMRate.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                             FMRate.SClass     = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                             FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                         IF NOT AVAILABLE FMRate THEN DO:
/*[EXECUTABLE]*/                                             FIND FIRST FMRate3 WHERE
/*[EXECUTABLE]*/                                                 FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
/*[EXECUTABLE]*/                                                 FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                                 FMRate3.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                                 FMRate3.SClass      = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                                 FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
/*[EXECUTABLE]*/                                                 NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                             IF NOT AVAILABLE FMRate THEN DO:
/*[EXECUTABLE]*/                                                 nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
/*[EXECUTABLE]*/                                                     " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
/*[EXECUTABLE]*/                                                     " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
/*[EXECUTABLE]*/                                             END.                                              
/*[EXECUTABLE]*/                                         END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                     ELSE DO:  
/*[EXECUTABLE]*/                                         IF EntS7072.CurrentTermAmt = "" THEN DO:
/*[EXECUTABLE]*/                                             FIND FIRST FMRate WHERE
/*[EXECUTABLE]*/                                                 FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/
/*[COMMENT]*/                                                    /*  FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/*/
/*[EXECUTABLE]*/                                                 FMRate.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                                 FMRate.SClass     = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                                 FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                             IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                                    /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                                    IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                                    RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                                    END.*/
/*[COMMENT]*/                                                    /*DISP  FMRate.SClass  FMRate.MinSI. */
/*[EXECUTABLE]*/                                                 FIND FIRST FMRate3 WHERE
/*[EXECUTABLE]*/                                                     FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
/*[EXECUTABLE]*/                                                     FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                                     FMRate3.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                                     FMRate3.SClass      = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                                     FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
/*[EXECUTABLE]*/                                                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                                 IF NOT AVAILABLE FMRate THEN DO:
/*[EXECUTABLE]*/                                                     nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
/*[EXECUTABLE]*/                                                         " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
/*[EXECUTABLE]*/                                                         " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
/*[EXECUTABLE]*/                                                 END.                                              
/*[EXECUTABLE]*/                                             END.
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                         ELSE DO:
/*[EXECUTABLE]*/                                             FIND FIRST FMRate WHERE
/*[COMMENT]*/                                                    /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
/*[EXECUTABLE]*/                                                 FMRate.prm_gap   = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/ 
/*[EXECUTABLE]*/                                                 FMRate.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                                 FMRate.SClass     = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                                 FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                             IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                                    /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                                    IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                                    RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                                    END.*/
/*[COMMENT]*/                                                    /*DISP  FMRate.SClass  FMRate.MinSI. */
/*[EXECUTABLE]*/                                                 FIND FIRST FMRate3 WHERE
/*[EXECUTABLE]*/                                                     FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/
/*[EXECUTABLE]*/                                                     FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                                     FMRate3.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                                     FMRate3.SClass      = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                                     FMRate3.CoverCode   = EntS7072.PolicyTypeCd   
/*[EXECUTABLE]*/                                                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                                 IF NOT AVAILABLE FMRate THEN DO:
/*[EXECUTABLE]*/                                                     nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
/*[EXECUTABLE]*/                                                         " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
/*[EXECUTABLE]*/                                                         " ·Ø¹ :" + EntS7072.COLLAmtAccident  + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd    .
/*[EXECUTABLE]*/                                                 END.                                              
/*[EXECUTABLE]*/                                             END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                     IF EntS7072.CompanyCode = "M82" AND nv_msgerrorgrp <> "" THEN DO:
/*[EXECUTABLE]*/                                         FIND FIRST FMRate WHERE
/*[COMMENT]*/                                            /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
/*[EXECUTABLE]*/                                         FMRate.prm_gap    = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                         FMRate.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                         FMRate.SClass     = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                         FMRate.CoverCode  = EntS7072.PolicyTypeCd    NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                         IF AVAILABLE FMRate THEN   nv_msgerrorgrp = "".
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                 END.
/*[EXECUTABLE]*/                                 ELSE DO:
/*[EXECUTABLE]*/                                     FIND FIRST FMRate WHERE
/*[COMMENT]*/                                            /*FMRate.prm_t      = deci(EntS7072.WrittenAmt)      AND /*àºÕéÂÊØ·¸Ô*/*/
/*[EXECUTABLE]*/                                         FMRate.prm_gap    = deci(EntS7072.CurrentTermAmt)  AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                         FMRate.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                         FMRate.SClass     = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                         FMRate.CoverCode  = EntS7072.PolicyTypeCd    AND
/*[EXECUTABLE]*/                                         FMRate.MinSI      >= nv_sumins                      AND
/*[EXECUTABLE]*/                                         FMRate.MaxSI      <= nv_sumins
/*[EXECUTABLE]*/                                         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                     IF NOT AVAILABLE FMRate THEN DO:
/*[COMMENT]*/                                            /*RUN pd_Prm_Gap.  /*prm_gap*/
/*[COMMENT]*/                                            IF nv_CheckData = NO THEN DO:
/*[COMMENT]*/                                            RUN pd_Prm_T.  /*prm_t*/
/*[COMMENT]*/                                            END.*/
/*[COMMENT]*/                                            /*DISP  FMRate.SClass  FMRate.MinSI. */
/*[EXECUTABLE]*/                                         FIND FIRST FMRate3 WHERE
/*[COMMENT]*/                                                /*FMRate3.prm_t       = deci(EntS7072.WrittenAmt)       AND /*àºÕéÂÊØ·¸Ô*/*/
/*[EXECUTABLE]*/                                             FMRate3.prm_gap     = deci(EntS7072.CurrentTermAmt)   AND /*àºÕéÂ+ÍÒ¡Ã+ÀÒÉÕ*/
/*[EXECUTABLE]*/                                             FMRate3.EffDate    <= TODAY                   AND
/*[EXECUTABLE]*/                                             FMRate3.SClass      = EntS7072.RateGroup       AND
/*[EXECUTABLE]*/                                             FMRate3.CoverCode   = EntS7072.PolicyTypeCd    AND
/*[EXECUTABLE]*/                                             FMRate3.MinSI      >= nv_sumins                     AND
/*[EXECUTABLE]*/                                             FMRate3.MaxSI      <= nv_sumins
/*[EXECUTABLE]*/                                             NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                         IF NOT AVAILABLE FMRate THEN DO:
/*[EXECUTABLE]*/                                             nv_msgerrorgrp = "äÁè¾ºá¾¤à¡¨àºÕéÂ»ÃÐ¡Ñ¹»ÃÐàÀ· : " + EntS7072.PolicyTypeCd + " ÃËÑÊ :" + EntS7072.RateGroup +
/*[EXECUTABLE]*/                                                 " àºÕéÂ:" +   EntS7072.WrittenAmt  + "/" + EntS7072.CurrentTermAmt +
/*[EXECUTABLE]*/                                                 " ·Ø¹ :" + EntS7072.COLLAmtAccident + " " +     EntS7072.Registration + " " + EntS7072.RegisteredProvCd .
/*[EXECUTABLE]*/                                         END.
/*[EXECUTABLE]*/                                     END.
/*[EXECUTABLE]*/                                 END.
/*[EXECUTABLE]*/                             END.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF nv_msgerrorgrp <> "" THEN DO:
/*[EXECUTABLE]*/                         OUTPUT TO CK_NotFMRateERROR.TXT APPEND .
/*[EXECUTABLE]*/                         PUT "/-----------------------------------------------/ "  SKIP. 
/*[EXECUTABLE]*/                         PUT  TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                             " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                         PUT "Company        : " EntS7072.CompanyCode        FORMAT "X(10)" SKIP. 
/*[EXECUTABLE]*/                         PUT "ContractNumber : " EntS7072.ContractNumber     FORMAT "X(35)" SKIP.   
/*[EXECUTABLE]*/                         PUT "PolicyTypeCd   : " EntS7072.PolicyTypeCd       FORMAT "X(10)" SKIP.   
/*[EXECUTABLE]*/                         PUT "RateGroup      : " EntS7072.RateGroup          FORMAT "X(10)" SKIP.   
/*[EXECUTABLE]*/                         PUT "WrittenAmt     : " EntS7072.WrittenAmt         FORMAT "x(20)" SKIP.   
/*[EXECUTABLE]*/                         PUT "CurrentTermAmt : " EntS7072.CurrentTermAmt     FORMAT "x(20)" SKIP.   
/*[EXECUTABLE]*/                         PUT "SumInsureAmt   : " EntS7072.SumInsureAmt       FORMAT "X(20)" SKIP.   
/*[EXECUTABLE]*/                         PUT "COLLAmtAccident: " EntS7072.COLLAmtAccident    FORMAT "X(20)" SKIP.   
/*[EXECUTABLE]*/                         PUT "FTAmt          : " EntS7072.FTAmt              FORMAT "X(20)" SKIP.  
/*[EXECUTABLE]*/                         PUT "Error :"           nv_msgerrorgrp              FORMAT "X(150)" SKIP.  
/*[EXECUTABLE]*/                         OUTPUT  CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SavePDFLKT C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SavePDFLKT :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /*add kridtiya i. A58-0356 */
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAIL IntS7072 THEN DO:
/*[EXECUTABLE]*/                     IF IntS7072.CompanyCode = "469" THEN DO:
/*[EXECUTABLE]*/                         CREATE ExtPDFLKT70.
/*[EXECUTABLE]*/                             ASSIGN 
/*[EXECUTABLE]*/                                 ExtPDFLKT70.SystemRq        = "Lockton" 
/*[EXECUTABLE]*/                                 ExtPDFLKT70.ContractNumber  = IntS7072.ContractNumber 
/*[EXECUTABLE]*/                                 ExtPDFLKT70.PolicyNumber    = IntS7072.Policy
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach1 = IntS7072.FileNameAttach1        
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach2 = IntS7072.FileNameAttach2        
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach3 = IntS7072.FileNameAttach3        
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach4 = IntS7072.FileNameAttach4        
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach5 = IntS7072.FileNameAttach5        
/*[EXECUTABLE]*/                                 ExtPDFLKT70.FileNameAttach6 = IntS7072.FileNameAttach6  
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile1     = IntS7072.AttachFile1         
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile2     = IntS7072.AttachFile2         
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile3     = IntS7072.AttachFile3         
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile4     = IntS7072.AttachFile4         
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile5     = IntS7072.AttachFile5         
/*[EXECUTABLE]*/                                 ExtPDFLKT70.AttachFile6     = IntS7072.AttachFile6 .
/*[BLANK]*/                                  
/*[BLANK]*/                              
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 RELEASE ExtPDFLKT70.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEPDResult C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVEPDResult :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:      ãªéÊÓËÃÑº µÍº¡ÅÑº àªè¹ ·Ø¹ àºÕéÂ ÍÒ¡Ã ÀÒÉÕ 
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 EntS7072Result.PolicyTypeCd                   = IntS7072.PolicyTypeCd
/*[EXECUTABLE]*/                 EntS7072Result.RateGroup                      = IntS7072.RateGroup
/*[EXECUTABLE]*/                 EntS7072Result.GarageTypeCd                   = IntS7072.GarageTypeCd
/*[EXECUTABLE]*/                 EntS7072Result.GarageDesc                     = IntS7072.GarageDesc
/*[EXECUTABLE]*/                 EntS7072Result.SumInsureAmt                   = IntS7072.SumInsureAmt
/*[EXECUTABLE]*/                 EntS7072Result.TPBIAmtPerson                  = IntS7072.TPBIAmtPerson
/*[EXECUTABLE]*/                 EntS7072Result.TPBIAmtAccident                = IntS7072.TPBIAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.PDAmtAccident                  = IntS7072.PDAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.DeductiblePDAmtAccident        = IntS7072.DeductiblePDAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.COLLAmtAccident                = IntS7072.COLLAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.DeductibleCOLLAmtAccident      = IntS7072.DeductibleCOLLAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.FTAmt                          = IntS7072.FTAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPADriverAmt              = IntS7072.PerilsPADriverAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPANumPassengers          = IntS7072.PerilsPANumPassengers
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPAPassengerAmt           = IntS7072.PerilsPAPassengerAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPATemporaryDriverAmt     = IntS7072.PerilsPATemporaryDriverAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPANumTemporaryPassengers = IntS7072.PerilsPANumTemporaryPassengers
/*[EXECUTABLE]*/                 EntS7072Result.PerilsPATemporaryPassengerAmt  = IntS7072.PerilsPATemporaryPassengerAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsMedicalTreatmentAmt      = IntS7072.PerilsMedicalTreatmentAmt
/*[EXECUTABLE]*/                 EntS7072Result.PerilsBailBondInsuranceAmt     = IntS7072.PerilsBailBondInsuranceAmt
/*[EXECUTABLE]*/                 EntS7072Result.PremiumCoverage13Amt           = IntS7072.PremiumCoverage13Amt
/*[EXECUTABLE]*/                 EntS7072Result.DiscountForNamedDriver         = IntS7072.DiscountForNamedDriver
/*[EXECUTABLE]*/                 EntS7072Result.DeductibleAmt                  = IntS7072.DeductibleAmt
/*[EXECUTABLE]*/                 EntS7072Result.FleetAmt                       = IntS7072.FleetAmt
/*[EXECUTABLE]*/                 EntS7072Result.GoodDriverIndPct               = IntS7072.GoodDriverIndPct
/*[EXECUTABLE]*/                 EntS7072Result.GoodDriverIndAmt               = IntS7072.GoodDriverIndAmt
/*[EXECUTABLE]*/                 EntS7072Result.TotalDiscountsAmt              = IntS7072.TotalDiscountsAmt
/*[EXECUTABLE]*/                 EntS7072Result.SurchargeFactorAmt             = IntS7072.SurchargeFactorAmt
/*[EXECUTABLE]*/                 EntS7072Result.PremiumCoverage2Amt            = IntS7072.PremiumCoverage2Amt
/*[EXECUTABLE]*/                 EntS7072Result.OtherDiscountAmt               = IntS7072.OtherDiscountAmt
/*[EXECUTABLE]*/                 EntS7072Result.VehicleUse                     = IntS7072.VehicleUse
/*[EXECUTABLE]*/                 EntS7072Result.WrittenAmt                     = IntS7072.WrittenAmt
/*[EXECUTABLE]*/                 EntS7072Result.RevenueStampAmt                = IntS7072.RevenueStampAmt
/*[EXECUTABLE]*/                 EntS7072Result.VatAmt                         = IntS7072.VatAmt
/*[EXECUTABLE]*/                 EntS7072Result.CurrentTermAmt                 = IntS7072.CurrentTermAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 EntS7072Result.CMIPolicyTypeCd                = IntS7072.CMIPolicyTypeCd
/*[EXECUTABLE]*/                 EntS7072Result.CMIVehTypeCd                   = IntS7072.CMIVehTypeCd
/*[EXECUTABLE]*/                 EntS7072Result.CMIAmtPerson                   = IntS7072.CMIAmtPerson
/*[EXECUTABLE]*/                 EntS7072Result.CMIAmtAccident                 = IntS7072.CMIAmtAccident
/*[EXECUTABLE]*/                 EntS7072Result.CMIWrittenAmt                  = IntS7072.CMIWrittenAmt
/*[EXECUTABLE]*/                 EntS7072Result.CMIRevenueStampAmt             = IntS7072.CMIRevenueStampAmt
/*[EXECUTABLE]*/                 EntS7072Result.CMIVatAmt                      = IntS7072.CMIVatAmt
/*[EXECUTABLE]*/                 EntS7072Result.CMICurrentTermAmt              = IntS7072.CMICurrentTermAmt .
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pc_SAVEProduction C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pc_SAVEProduction :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_AddData        AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Cpolicy        AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CBarCodeNumber AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ChkCpLink      AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CallbyLink     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LinkFile       AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SwithChk       AS LOGICAL   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_recinout7072   AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVEERROR      AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_companyno      AS CHARACTER INIT "".
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                     nv_companyno  = ""
/*[EXECUTABLE]*/                 nv_SAVEERROR      = ""
/*[EXECUTABLE]*/                 nv_Cpolicy        = ""
/*[EXECUTABLE]*/                 nv_CBarCodeNumber = ""
/*[EXECUTABLE]*/                 nv_resulttext     = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                   ASSIGN   
/*[EXECUTABLE]*/                     nv_companyno = IntS7072.CompanyCode
/*[EXECUTABLE]*/                     nv_AddData   = NO.
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
/*[EXECUTABLE]*/                   IF nv_PolicyV70 = "" THEN nv_PolicyV70 = nv_PolicyV72.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST IntPol7072 WHERE
/*[EXECUTABLE]*/                              IntPol7072.PolicyNumber = nv_PolicyV70  /*IntS7072.PolicyNumber*/
/*[EXECUTABLE]*/                          AND IntPol7072.CompanyCode  = IntS7072.CompanyCode
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF NOT AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     nv_AddData = YES.
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
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[BLANK]*/                          
/*[EXECUTABLE]*/                     ASSIGN 
/*[COMMENT]*/                        /* IntS7072.Statusflag = "U"  UPDATE DATA*/
/*[EXECUTABLE]*/                     nv_Cpolicy = "PolicyNumber" 
/*[COMMENT]*/                        /* IF IntS7072.Statusflag <> "" THEN  */
/*[EXECUTABLE]*/                     nv_AddData = YES.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_AddData = YES THEN DO:
/*[EXECUTABLE]*/                     nv_covcodtyp1 = ""  . /*Add Kridtiya i. clear */
/*[EXECUTABLE]*/                     nv_recinout7072 = 0.
/*[EXECUTABLE]*/                     RUN PD_SAVEPD1 (nv_rec_rq
/*[EXECUTABLE]*/                                    ,nv_PolicyV70 /*,nv_DocnoV70*/
/*[EXECUTABLE]*/                                    ,nv_PolicyV72 /*,nv_DocnoV72*/
/*[EXECUTABLE]*/                                    ,INPUT-OUTPUT nv_recinout7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_companyno = "469" THEN RUN Pc_SavePDFLKT.  /*Kridtiya i. Date. 17/07/2017*/ 
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                     NO-ERROR NO-WAIT.
/*[COMMENT]*/                        /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_recinout7072
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF IntPol7072.GenSicBranST = "ERROR" THEN DO:
/*[EXECUTABLE]*/                         nv_SAVEERROR = IntPol7072.ErrorMessage.
/*[EXECUTABLE]*/                         nv_AddData = NO.
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[COMMENT]*/                          /*§Ò¹ "R" µèÍÍÒÂØ->¡¸. ÁÕ¡ÒÃà»ÅÕèÂ¹àºÍÃì¢éÒ§ã¹*/
/*[EXECUTABLE]*/                       IF TRIM(IntPol7072.PolicyNumber) <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         nv_PolicyV70 = IntPol7072.PolicyNumber.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END. /*IF nv_AddData = YES THEN DO:*/
/*[COMMENT]*/                      /* ----------------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_AddData = YES THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST IntPol7072Result WHERE
/*[EXECUTABLE]*/                                IntPol7072Result.CompanyCode  = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND IntPol7072Result.PolicyNumber = nv_PolicyV70
/*[EXECUTABLE]*/                     NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE IntPol7072Result THEN  CREATE IntPol7072Result.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     IntPol7072Result.CompanyCode      = IntS7072.CompanyCode
/*[EXECUTABLE]*/                     IntPol7072Result.ReferenceNumber  = IntS7072.ReferenceNumber
/*[EXECUTABLE]*/                     IntPol7072Result.EndorseRefNumber = IntS7072.EndorseRefNumber
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.InsurerCode    = IntS7072.InsurerCode
/*[EXECUTABLE]*/                     IntPol7072Result.MethodCode     = IntS7072.MethodCode
/*[EXECUTABLE]*/                     IntPol7072Result.Policy         = IntS7072.Policy
/*[EXECUTABLE]*/                     IntPol7072Result.Rencnt         = IntS7072.Rencnt
/*[EXECUTABLE]*/                     IntPol7072Result.Endcnt         = IntS7072.Endcnt
/*[EXECUTABLE]*/                     IntPol7072Result.Riskno         = IntS7072.Riskno
/*[EXECUTABLE]*/                     IntPol7072Result.Itemno         = IntS7072.Itemno
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.ProcessByUser  = IntS7072.ProcessByUser
/*[EXECUTABLE]*/                     IntPol7072Result.ProcessDate    = IntS7072.ProcessDate
/*[EXECUTABLE]*/                     IntPol7072Result.ProcessTime    = IntS7072.ProcessTime
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                     IntPol7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
/*[EXECUTABLE]*/                     IntPol7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
/*[EXECUTABLE]*/                     IntPol7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                     IntPol7072Result.InsurerId       = IntS7072.InsurerId
/*[EXECUTABLE]*/                     IntPol7072Result.ContractNumber  = IntS7072.ContractNumber
/*[EXECUTABLE]*/                     IntPol7072Result.RqUID           = nv_octets
/*[EXECUTABLE]*/                     IntPol7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IntPol7072Result.RecordGUIDRs    = nv_octets
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
/*[EXECUTABLE]*/                                                              + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                                                              + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                     IntPol7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.PolicyNumber    = nv_PolicyV70
/*[EXECUTABLE]*/                     IntPol7072Result.DocumentUID     = IntS7072.DocumentUID
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.CMIPolicyNumber = nv_PolicyV72
/*[EXECUTABLE]*/                     IntPol7072Result.CMIDocumentUID  = IntS7072.CMIDocumentUID
/*[EXECUTABLE]*/                     IntPol7072Result.MsgStatusCd     = "SUCCESS" 
/*[BLANK]*/                          
/*[COMMENT]*/                        /*IntPol7072Result.CMIBarCodeNumber = IntS7072.CMIBarCodeNumber */
/*[EXECUTABLE]*/                     IntPol7072Result.ResultStatus     = "SUCCESS"                 /*IntS7072.ResultStatus*/
/*[EXECUTABLE]*/                     IntPol7072Result.ErrorCode        = ""
/*[EXECUTABLE]*/                     IntPol7072Result.ErrorMessage     = nv_resulttext
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IntPol7072Result.vehreg           = IntS7072.vehreg
/*[EXECUTABLE]*/                     IntPol7072Result.Adjustno         = 0
/*[EXECUTABLE]*/                     IntPol7072Result.ProcessStatus    = "X" .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   END. /*IF nv_AddData = YES THEN DO:*/
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
/*[EXECUTABLE]*/                   IF nv_AddData = YES THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST EntS7072Result WHERE
/*[EXECUTABLE]*/                                EntS7072Result.SystemRq     = IntS7072.SystemRq
/*[EXECUTABLE]*/                            AND EntS7072Result.CompanyCode  = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND EntS7072Result.PolicyNumber = nv_PolicyV70
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                          /*copy link*/
/*[EXECUTABLE]*/                       nv_ChkCpLink = NO.
/*[EXECUTABLE]*/                       nv_SwithChk  = NO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF IntS7072.FileNameAttach1 <> "" AND IntS7072.AttachFile1 <> ?
/*[EXECUTABLE]*/                          AND IntS7072.CompanyCode <> "833" /*ÈÃÕ¡ÃØ§*/
/*[EXECUTABLE]*/                       THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         FIND FIRST ExtInsurerCd WHERE
/*[EXECUTABLE]*/                                    ExtInsurerCd.InsurerCd = IntS7072.CompanyCode
/*[EXECUTABLE]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE ExtInsurerCd THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF ExtInsurerCd.CallbyLink <> "" THEN DO: /*ÁÕ¡ÒÃ copy Link*/
/*[EXECUTABLE]*/                             nv_ChkCpLink  = YES.
/*[EXECUTABLE]*/                             nv_CallbyLink = ExtInsurerCd.CallbyLink.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN WRS/WRSCpLink.P (RECID(IntS7072)
/*[EXECUTABLE]*/                                                 ,nv_PolicyV70
/*[EXECUTABLE]*/                                                 ,nv_PolicyV72
/*[EXECUTABLE]*/                                                 ,nv_octets
/*[EXECUTABLE]*/                                                 ,nv_resulttext).
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                       NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       CREATE EntS7072Result.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       EntS7072Result.ReferenceNumber  = IntS7072.ReferenceNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseRefNumber = IntS7072.EndorseRefNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.InsurerCode = IntS7072.InsurerCode
/*[EXECUTABLE]*/                       EntS7072Result.MethodCode  = IntS7072.MethodCode
/*[EXECUTABLE]*/                       EntS7072Result.Policy      = IntS7072.Policy
/*[EXECUTABLE]*/                       EntS7072Result.Rencnt      = IntS7072.Rencnt
/*[EXECUTABLE]*/                       EntS7072Result.Endcnt      = IntS7072.Endcnt
/*[EXECUTABLE]*/                       EntS7072Result.Riskno      = IntS7072.Riskno
/*[EXECUTABLE]*/                       EntS7072Result.Itemno      = IntS7072.Itemno
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.ProcessByUser = IntS7072.ProcessByUser
/*[EXECUTABLE]*/                       EntS7072Result.ProcessDate   = IntS7072.ProcessDate
/*[EXECUTABLE]*/                       EntS7072Result.ProcessTime   = IntS7072.ProcessTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
/*[EXECUTABLE]*/                       EntS7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.RecordGUIDRs = nv_octets
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
/*[EXECUTABLE]*/                                                              + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                                                              + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.PolicyNumber     = nv_PolicyV70
/*[EXECUTABLE]*/                       EntS7072Result.DocumentUID      = IntS7072.DocumentUID
/*[EXECUTABLE]*/                       EntS7072Result.StickerNumber    = IntS7072.StickerNumber
/*[EXECUTABLE]*/                       EntS7072Result.CMIPolicyNumber  = nv_PolicyV72
/*[EXECUTABLE]*/                       EntS7072Result.CMIDocumentUID   = IntS7072.CMIDocumentUID
/*[EXECUTABLE]*/                       EntS7072Result.CMIBarCodeNumber = IntS7072.CMIBarCodeNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.MsgStatusCd  = "SUCCESS"
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.BranchCd     = IntS7072.BranchCd
/*[EXECUTABLE]*/                       EntS7072Result.vehreg       = IntS7072.vehreg
/*[EXECUTABLE]*/                       EntS7072Result.Adjustno     = 0
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.ResultStatus = "SUCCESS"                 /*IntS7072.ResultStatus*/
/*[EXECUTABLE]*/                       EntS7072Result.ErrorCode    = ""
/*[EXECUTABLE]*/                       EntS7072Result.ErrorMessage = nv_resulttext
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile1  = IntS7072.AttachFile1
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile2  = IntS7072.AttachFile2
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile3  = IntS7072.AttachFile3
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile4  = IntS7072.AttachFile4
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile5  = IntS7072.AttachFile5
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile6  = IntS7072.AttachFile6
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile7  = IntS7072.AttachFile7
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile8  = IntS7072.AttachFile8
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile9  = IntS7072.AttachFile9
/*[EXECUTABLE]*/                       EntS7072Result.AttachFile10 = IntS7072.AttachFile10
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach1  = IntS7072.FileNameAttach1
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach2  = IntS7072.FileNameAttach2
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach3  = IntS7072.FileNameAttach3
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach4  = IntS7072.FileNameAttach4
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach5  = IntS7072.FileNameAttach5
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach6  = IntS7072.FileNameAttach6
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach7  = IntS7072.FileNameAttach7
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach8  = IntS7072.FileNameAttach8
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach9  = IntS7072.FileNameAttach9
/*[EXECUTABLE]*/                       EntS7072Result.FileNameAttach10 = IntS7072.FileNameAttach10
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.LinkStatus       = "" .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF EntS7072Result.FileNameAttach1 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach1.  /*FILE NAME PDF*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         EntS7072Result.LinkPolicy = nv_LinkFile.
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF EntS7072Result.FileNameAttach2 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach2.  /*FILE NAME PDF*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         EntS7072Result.LinkFileAttach1 = nv_LinkFile.
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF EntS7072Result.FileNameAttach3 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         nv_LinkFile = TRIM(nv_CallbyLink) + "/" + EntS7072Result.FileNameAttach3.  /*FILE NAME PDF*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         EntS7072Result.LinkFileAttach2 = nv_LinkFile.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* ----------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN Pc_SAVEPDResult. /*µÍº¡ÅÑº ·Ø¹ àºÕéÂ ÍÒ¡Ã ÀÒÉÕ*/
/*[COMMENT]*/                          /* ----------- */
/*[BLANK]*/                      
/*[COMMENT]*/                          /*äÁèÁÕ¡ÒÃ copy Link*/
/*[EXECUTABLE]*/                       IF nv_ChkCpLink = NO THEN EntS7072Result.LinkStatus = "X" .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       EntS7072Result.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                       EntS7072Result.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                       EntS7072Result.InsurerId       = IntS7072.InsurerId
/*[EXECUTABLE]*/                       EntS7072Result.ContractNumber  = IntS7072.ContractNumber
/*[EXECUTABLE]*/                       EntS7072Result.RqUID           = nv_octets
/*[EXECUTABLE]*/                       EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq .
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
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST EntS7072 WHERE
/*[EXECUTABLE]*/                                EntS7072.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                            AND EntS7072.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND EntS7072.Username        = IntS7072.Username
/*[EXECUTABLE]*/                            AND EntS7072.Password        = IntS7072.Password
/*[EXECUTABLE]*/                            AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                     NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "X" .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IntS7072.ProcessStatus = "X".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   END. /*IF nv_AddData = YES THEN DO:*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_AddData = NO THEN DO:  /*Duplication*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST EntS7072Result WHERE
/*[EXECUTABLE]*/                                EntS7072Result.SystemRq     = IntS7072.SystemRq
/*[EXECUTABLE]*/                            AND EntS7072Result.CompanyCode  = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND EntS7072Result.PolicyNumber = nv_PolicyV70
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       CREATE EntS7072Result.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       EntS7072Result.ReferenceNumber = IntS7072.ReferenceNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseRefNumber= IntS7072.EndorseRefNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.InsurerCode = IntS7072.InsurerCode
/*[EXECUTABLE]*/                       EntS7072Result.MethodCode  = IntS7072.MethodCode
/*[EXECUTABLE]*/                       EntS7072Result.Policy      = IntS7072.Policy
/*[EXECUTABLE]*/                       EntS7072Result.Rencnt      = IntS7072.Rencnt
/*[EXECUTABLE]*/                       EntS7072Result.Endcnt      = IntS7072.Endcnt
/*[EXECUTABLE]*/                       EntS7072Result.Riskno      = IntS7072.Riskno
/*[EXECUTABLE]*/                       EntS7072Result.Itemno      = IntS7072.Itemno
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.ProcessByUser = IntS7072.ProcessByUser
/*[EXECUTABLE]*/                       EntS7072Result.ProcessDate   = IntS7072.ProcessDate
/*[EXECUTABLE]*/                       EntS7072Result.ProcessTime   = IntS7072.ProcessTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntDate = IntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntTime = IntS7072.TrnFromIntTime
/*[EXECUTABLE]*/                       EntS7072Result.ReceiveNumber  = IntS7072.ReceiveNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.RecordGUIDRs = ""
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseDt   = STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
/*[EXECUTABLE]*/                                                              + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                                                              + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.PolicyNumber  = nv_PolicyV70
/*[EXECUTABLE]*/                       EntS7072Result.DocumentUID   = ""
/*[EXECUTABLE]*/                       EntS7072Result.StickerNumber = ""
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.CMIPolicyNumber  = nv_PolicyV72
/*[EXECUTABLE]*/                       EntS7072Result.CMIDocumentUID   = ""
/*[EXECUTABLE]*/                       EntS7072Result.CMIBarCodeNumber = "" 
/*[EXECUTABLE]*/                       EntS7072Result.MsgStatusCd      = "FAIL"
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.BranchCd = IntS7072.BranchCd
/*[EXECUTABLE]*/                       EntS7072Result.vehreg   = IntS7072.vehreg
/*[EXECUTABLE]*/                       EntS7072Result.Adjustno = 0
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.ResultStatus = "FAIL"                 /*IntS7072.ResultStatus*/
/*[EXECUTABLE]*/                       EntS7072Result.ErrorCode    = ""
/*[EXECUTABLE]*/                       EntS7072Result.ErrorMessage = "" 
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.LinkStatus   = "E"     .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_Cpolicy <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         FIND FIRST EntErrorCd WHERE
/*[EXECUTABLE]*/                                    EntErrorCd.SystemCode = "CMIPolicy"
/*[EXECUTABLE]*/                              AND   EntErrorCd.ErrorField = nv_Cpolicy
/*[EXECUTABLE]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE EntErrorCd THEN
/*[EXECUTABLE]*/                            ASSIGN EntS7072Result.ErrorCode    = EntErrorCd.ErrorCode
/*[EXECUTABLE]*/                                   EntS7072Result.ErrorMessage = EntErrorCd.ErrorMessage .
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_CBarCodeNumber <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         FIND FIRST EntErrorCd WHERE
/*[EXECUTABLE]*/                                    EntErrorCd.SystemCode = "CMIPolicy"
/*[EXECUTABLE]*/                              AND   EntErrorCd.ErrorField = nv_CBarCodeNumber
/*[EXECUTABLE]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE EntErrorCd THEN
/*[EXECUTABLE]*/                            ASSIGN EntS7072Result.ErrorCode    = EntErrorCd.ErrorCode
/*[EXECUTABLE]*/                                   EntS7072Result.ErrorMessage = EntErrorCd.ErrorMessage .
/*[EXECUTABLE]*/                       END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF EntS7072Result.ErrorMessage = "" THEN EntS7072Result.ErrorMessage = nv_SAVEERROR.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       EntS7072Result.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                       EntS7072Result.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                       EntS7072Result.InsurerId       = IntS7072.InsurerId
/*[EXECUTABLE]*/                       EntS7072Result.ContractNumber  = IntS7072.ContractNumber
/*[EXECUTABLE]*/                       EntS7072Result.RqUID           = IntS7072.RqUID
/*[EXECUTABLE]*/                       EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST EntS7072 WHERE
/*[EXECUTABLE]*/                                EntS7072.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                            AND EntS7072.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND EntS7072.Username        = IntS7072.Username
/*[EXECUTABLE]*/                            AND EntS7072.Password        = IntS7072.Password
/*[EXECUTABLE]*/                            AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                     NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "X" .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   END. /*IF nv_AddData = NO THEN DO:*/
/*[COMMENT]*/                      /* ----------------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO PUT_IntS7072.TXT.
/*[EXECUTABLE]*/                   PUT "                  |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                       " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                   PUT "MethodCode = SPLIT|" IntS7072.MethodCode  SKIP.
/*[EXECUTABLE]*/                   PUT "3  CompanyCode    |" IntS7072.CompanyCode SKIP.
/*[EXECUTABLE]*/                   PUT "3  ContractNumber |" IntS7072.ContractNumber FORMAT "x(30)" SKIP.
/*[EXECUTABLE]*/                   PUT "5  PolicyNumber   |" nv_PolicyV70 FORMAT "x(20)" SKIP.
/*[EXECUTABLE]*/                   PUT "6  BarCodeNumber  |" IntS7072.StickerNumber SKIP.
/*[EXECUTABLE]*/                   PUT "7  nv_DocnoV70    |" nv_DocnoV70  FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                   PUT "8  EffectiveDt    |" IntS7072.EffectiveDt SKIP.
/*[EXECUTABLE]*/                   PUT "9  PlateNumber    |" IntS7072.PlateNumber SKIP.
/*[EXECUTABLE]*/                   PUT "10 VehicleTypeCode|" IntS7072.VehicleTypeCode SKIP.
/*[EXECUTABLE]*/                   PUT "24 InsuredName    |" IntS7072.InsuredName     SKIP.
/*[EXECUTABLE]*/                   PUT "25 InsuredSurname |" IntS7072.InsuredSurname  SKIP.
/*[EXECUTABLE]*/                   PUT "30 PolicyTypeCd   |" IntS7072.PolicyTypeCd FORMAT "x(5)" SKIP.
/*[EXECUTABLE]*/                   PUT "31 RateGroup      |" IntS7072.RateGroup  SKIP.
/*[EXECUTABLE]*/                   PUT "27 WrittenAmt     |" IntS7072.WrittenAmt SKIP.
/*[EXECUTABLE]*/                   PUT "5  CMIPolicyNumber|" nv_PolicyV72 FORMAT "x(20)" SKIP.
/*[EXECUTABLE]*/                   PUT "7  nv_DocnoV72    |" nv_DocnoV72  FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                   PUT "32 CMIPolicyTypeCd|" IntS7072.CMIPolicyTypeCd SKIP.
/*[EXECUTABLE]*/                   PUT "33 CMIVehTypeCd   |" IntS7072.CMIVehTypeCd SKIP.
/*[EXECUTABLE]*/                   PUT  FILL("-",78) FORMAT "X(78)" SKIP(1).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   PUT "FileNameAttach1|" IntS7072.FileNameAttach1 SKIP.
/*[EXECUTABLE]*/                   PUT "FileNameAttach2|" IntS7072.FileNameAttach2 SKIP.
/*[EXECUTABLE]*/                   PUT "FileNameAttach3|" IntS7072.FileNameAttach3 SKIP.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   PUT  FILL("-",78) FORMAT "X(78)" SKIP(1).
/*[EXECUTABLE]*/                   OUTPUT  CLOSE.
/*[EXECUTABLE]*/                 END. /*IF AVAILABLE IntS7072 THEN DO: */
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RELEASE EntS7072Result.
/*[EXECUTABLE]*/                 RELEASE EntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RELEASE IntS7072.
/*[EXECUTABLE]*/                 RELEASE IntPol7072Result.
/*[EXECUTABLE]*/                 RELEASE IntPol7072.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkBlankForm C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ChkBlankForm :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_PrgName       AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF IntPol7072.CompanyCode = "833" THEN DO:
/*[BLANK]*/                          
/*[EXECUTABLE]*/                       IF (IntPol7072.CMIDocumentUID   >= "7545177"       AND IntPol7072.CMIDocumentUID   <= "7545276")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIDocumentUID   >= "7549101"       AND IntPol7072.CMIDocumentUID   <= "7549171")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIDocumentUID   >= "7842501"       AND IntPol7072.CMIDocumentUID   <= "7845000")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIDocumentUID   >= "7840001"       AND IntPol7072.CMIDocumentUID   <= "7842500")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIDocumentUID   >= "7845001"       AND IntPol7072.CMIDocumentUID   <= "7845500")
/*[COMMENT]*/                          /* TEST
/*[COMMENT]*/                          OR (IntPol7072.CMIDocumentUID   >= "8330201"       AND IntPol7072.CMIDocumentUID   <= "8330300") */
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIBarCodeNumber >= "0210021101776" AND IntPol7072.CMIBarCodeNumber <= "0210021102760")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIBarCodeNumber >= "0210021141013" AND IntPol7072.CMIBarCodeNumber <= "0210021141713")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIBarCodeNumber >= "0210022425012" AND IntPol7072.CMIBarCodeNumber <= "0210022450002")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIBarCodeNumber >= "0210022400011" AND IntPol7072.CMIBarCodeNumber <= "0210022425001")
/*[EXECUTABLE]*/                       OR (IntPol7072.CMIBarCodeNumber >= "0210022450013" AND IntPol7072.CMIBarCodeNumber <= "0210022455005")
/*[COMMENT]*/                          /* TEST
/*[COMMENT]*/                          OR (IntPol7072.CMIBarCodeNumber >= "3595154544881" AND IntPol7072.CMIBarCodeNumber <= "3695185266662") */
/*[EXECUTABLE]*/                       THEN DO:
/*[COMMENT]*/                            /*MESSAGE "OLD " SKIP(1)
/*[COMMENT]*/                                      IntPol7072.CMIDocumentUID SKIP
/*[COMMENT]*/                                      IntPol7072.CMIBarCodeNumber SKIP (1) 
/*[COMMENT]*/                              VIEW-AS ALERT-BOX.*/
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       ELSE DO:
/*[COMMENT]*/                            /*Blank form Compulsory */
/*[BLANK]*/                      
/*[COMMENT]*/                            /*MESSAGE "NEW " SKIP(1)
/*[COMMENT]*/                                      IntPol7072.CMIDocumentUID SKIP
/*[COMMENT]*/                                      IntPol7072.CMIBarCodeNumber SKIP (1) 
/*[COMMENT]*/                              VIEW-AS ALERT-BOX.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         FIND FIRST FNameAttach WHERE
/*[EXECUTABLE]*/                                    FNameAttach.CompanyCode  = TRIM(IntPol7072.CompanyCode) + "_2"
/*[EXECUTABLE]*/                                AND FNameAttach.PolicyTypeCd = "V72"
/*[EXECUTABLE]*/                                AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
/*[EXECUTABLE]*/                                AND FNameAttach.EffDate     <= TODAY
/*[EXECUTABLE]*/                                AND FNameAttach.SelectNumber = 1
/*[EXECUTABLE]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE FNameAttach THEN
/*[EXECUTABLE]*/                           ASSIGN
/*[EXECUTABLE]*/                           nv_NameCompCd = FNameAttach.CompanyCode /*833_2*/
/*[EXECUTABLE]*/                           nv_PrgName    = FNameAttach.PrgName
/*[EXECUTABLE]*/                           nv_PrmPrg     = FNameAttach.PrmPrg. /*cmipolicy2*/
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt1 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ChkDataExt1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_Mdocno1 AS CHARACTER NO-UNDO. /*M*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_Tdocno1 AS CHARACTER NO-UNDO. /*T*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PolicyTypeCd AS CHARACTER INITIAL "" NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVEError    AS CHARACTER INITIAL "" NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 CREATE IntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.Username             = EntS7072.Username
/*[EXECUTABLE]*/                 IntS7072.Password             = EntS7072.Password
/*[EXECUTABLE]*/                 IntS7072.CompanyCode          = EntS7072.CompanyCode
/*[EXECUTABLE]*/                 IntS7072.BranchCd             = EntS7072.BranchCd 
/*[EXECUTABLE]*/                 IntS7072.InsurerId            = EntS7072.InsurerId
/*[EXECUTABLE]*/                 IntS7072.InsuranceCd          = EntS7072.InsuranceCd
/*[EXECUTABLE]*/                 IntS7072.InsuranceBranchCd    = EntS7072.InsuranceBranchCd
/*[EXECUTABLE]*/                 IntS7072.PolicyStatus         = EntS7072.PolicyStatus
/*[EXECUTABLE]*/                 IntS7072.ContractNumber       = EntS7072.ContractNumber
/*[EXECUTABLE]*/                 IntS7072.ContractDt           = EntS7072.ContractDt
/*[EXECUTABLE]*/                 IntS7072.ContractTime         = EntS7072.ContractTime
/*[EXECUTABLE]*/                 IntS7072.CMVApplicationNumber = EntS7072.CMVApplicationNumber
/*[EXECUTABLE]*/                 IntS7072.ApplicationNumber    = EntS7072.ApplicationNumber  /* QNumPremium */
/*[EXECUTABLE]*/                 IntS7072.ApplicationDt        = EntS7072.ApplicationDt
/*[EXECUTABLE]*/                 IntS7072.ApplicationTime      = EntS7072.ApplicationTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.InsuredType                 = EntS7072.InsuredType
/*[EXECUTABLE]*/                 IntS7072.InsuredUniqueID             = REPLACE(EntS7072.InsuredUniqueID,CHR(13),"") 
/*[EXECUTABLE]*/                 IntS7072.InsuredUniqueIDExpDt        = EntS7072.InsuredUniqueIDExpDt
/*[EXECUTABLE]*/                 IntS7072.InsuredUniqueIDExpirationDt = EntS7072.InsuredUniqueIDExpirationDt
/*[EXECUTABLE]*/                 IntS7072.License        = EntS7072.License
/*[EXECUTABLE]*/                 IntS7072.BirthDt        = EntS7072.BirthDt
/*[EXECUTABLE]*/                 IntS7072.InsuredCd      = EntS7072.InsuredCd
/*[EXECUTABLE]*/                 IntS7072.InsuredTitle   = EntS7072.InsuredTitle
/*[COMMENT]*/                        /*
/*[COMMENT]*/                    IntS7072.InsuredName    = EntS7072.InsuredName                        /*Add by Kridtiya i. Date.02/02/2017*/
/*[EXECUTABLE]*/                 IntS7072.InsuredSurname = EntS7072.InsuredSurname*/                   /*Add by Kridtiya i. Date.02/02/2017*/
/*[EXECUTABLE]*/                 IntS7072.InsuredName    = replace(EntS7072.InsuredName,CHR(26),"")    /*Add by Kridtiya i. Date.02/02/2017*/
/*[EXECUTABLE]*/                 IntS7072.InsuredSurname = replace(EntS7072.InsuredSurname,CHR(26),"") /*Add by Kridtiya i. Date.02/02/2017*/
/*[EXECUTABLE]*/                 IntS7072.InsuredBranch  = EntS7072.InsuredBranch
/*[EXECUTABLE]*/                 IntS7072.Addr           = EntS7072.Addr
/*[EXECUTABLE]*/                 IntS7072.UnitNumber     = EntS7072.UnitNumber
/*[EXECUTABLE]*/                 IntS7072.RoomNumber     = EntS7072.RoomNumber
/*[EXECUTABLE]*/                 IntS7072.Building       = EntS7072.Building
/*[EXECUTABLE]*/                 IntS7072.VillageNumber  = EntS7072.VillageNumber
/*[EXECUTABLE]*/                 IntS7072.Alley          = EntS7072.Alley
/*[EXECUTABLE]*/                 IntS7072.Lane           = EntS7072.Lane
/*[EXECUTABLE]*/                 IntS7072.StreetName     = EntS7072.StreetName
/*[EXECUTABLE]*/                 IntS7072.SubDistrict    = EntS7072.SubDistrict
/*[EXECUTABLE]*/                 IntS7072.District       = EntS7072.District
/*[EXECUTABLE]*/                 IntS7072.StateProvCd    = EntS7072.StateProvCd
/*[EXECUTABLE]*/                 IntS7072.StateProv      = EntS7072.StateProv
/*[EXECUTABLE]*/                 IntS7072.Province       = EntS7072.Province
/*[EXECUTABLE]*/                 IntS7072.PostalCode     = EntS7072.PostalCode
/*[EXECUTABLE]*/                 IntS7072.OccupationDesc = EntS7072.OccupationDesc
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.MobilePhoneNumber = EntS7072.MobilePhoneNumber
/*[EXECUTABLE]*/                 IntS7072.MobileNumber      = EntS7072.MobileNumber
/*[EXECUTABLE]*/                 IntS7072.PhoneNumber       = EntS7072.PhoneNumber
/*[EXECUTABLE]*/                 IntS7072.OfficePhoneNumber = EntS7072.OfficePhoneNumber
/*[EXECUTABLE]*/                 IntS7072.EmailAddr         = EntS7072.EmailAddr
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.ReceiptName = EntS7072.ReceiptName
/*[EXECUTABLE]*/                 IntS7072.ReceiptAddr = EntS7072.ReceiptAddr
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.DriverNameCd  = EntS7072.DriverNameCd
/*[EXECUTABLE]*/                 IntS7072.InsuredTitle1 = EntS7072.InsuredTitle1
/*[EXECUTABLE]*/                 IntS7072.InsuredName1  = EntS7072.InsuredName1
/*[EXECUTABLE]*/                 IntS7072.InsuredSurname1 = EntS7072.InsuredSurname1
/*[EXECUTABLE]*/                 IntS7072.OccupationDesc1 = EntS7072.OccupationDesc1
/*[EXECUTABLE]*/                 IntS7072.BirthDt1 = EntS7072.BirthDt1
/*[EXECUTABLE]*/                 IntS7072.InsuredUniqueID1 = EntS7072.InsuredUniqueID1
/*[EXECUTABLE]*/                 IntS7072.License1 = EntS7072.License1
/*[EXECUTABLE]*/                 IntS7072.InsuredTitle2 = EntS7072.InsuredTitle2
/*[EXECUTABLE]*/                 IntS7072.InsuredName2  = EntS7072.InsuredName2
/*[EXECUTABLE]*/                 IntS7072.InsuredSurname2 = EntS7072.InsuredSurname2
/*[EXECUTABLE]*/                 IntS7072.OccupationDesc2 = EntS7072.OccupationDesc2
/*[EXECUTABLE]*/                 IntS7072.BirthDt2 = EntS7072.BirthDt2
/*[EXECUTABLE]*/                 IntS7072.InsuredUniqueID2 = EntS7072.InsuredUniqueID2
/*[EXECUTABLE]*/                 IntS7072.License2  = EntS7072.License2
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.Beneficiaries = EntS7072.Beneficiaries
/*[EXECUTABLE]*/                 IntS7072.PolicyAttachment = EntS7072.PolicyAttachment
/*[EXECUTABLE]*/                 IntS7072.VehicleUse = EntS7072.VehicleUse
/*[EXECUTABLE]*/                 IntS7072.PromptText = EntS7072.PromptText
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.VehGroup        = EntS7072.VehGroup
/*[EXECUTABLE]*/                 IntS7072.VehTypeCd       = EntS7072.VehTypeCd
/*[EXECUTABLE]*/                 IntS7072.VehicleTypeCode = EntS7072.VehicleTypeCode
/*[EXECUTABLE]*/                 IntS7072.Manufacturer    = REPLACE(EntS7072.Manufacturer,   CHR(13),"")  
/*[EXECUTABLE]*/                 IntS7072.Model           = REPLACE(EntS7072.Model,          CHR(13),"") 
/*[EXECUTABLE]*/                 IntS7072.ModelTypeName   = EntS7072.ModelTypeName           
/*[EXECUTABLE]*/                 IntS7072.ModelYear       = EntS7072.ModelYear               
/*[EXECUTABLE]*/                 IntS7072.VehBodyTypeDesc = REPLACE(EntS7072.VehBodyTypeDesc,CHR(13),"") 
/*[EXECUTABLE]*/                 IntS7072.SeatingCapacity = EntS7072.SeatingCapacity
/*[EXECUTABLE]*/                 IntS7072.Displacement    = EntS7072.Displacement
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.GrossVehOrCombinedWeight = EntS7072.GrossVehOrCombinedWeight
/*[EXECUTABLE]*/                 IntS7072.ColourCd = EntS7072.ColourCd
/*[EXECUTABLE]*/                 IntS7072.Colour   = EntS7072.Colour
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.ChassisVINNumber    = EntS7072.ChassisVINNumber
/*[EXECUTABLE]*/                 IntS7072.ChassisSerialNumber = EntS7072.ChassisSerialNumber
/*[EXECUTABLE]*/                 IntS7072.EngineSerialNumber  = EntS7072.EngineSerialNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.Registration           = EntS7072.Registration
/*[EXECUTABLE]*/                 IntS7072.RegisteredProvCd       = EntS7072.RegisteredProvCd
/*[EXECUTABLE]*/                 IntS7072.PlateNumber            = EntS7072.PlateNumber
/*[EXECUTABLE]*/                 IntS7072.RegisteredProvinceCode = EntS7072.RegisteredProvinceCode
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.RegisteredYear = EntS7072.RegisteredYear
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.SumInsureAmt = EntS7072.SumInsureAmt .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.PolicyTypeCd = EntS7072.PolicyTypeCd
/*[EXECUTABLE]*/                 IntS7072.RateGroup    = EntS7072.RateGroup
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.PolicyNumber = EntS7072.PolicyNumber
/*[EXECUTABLE]*/                 IntS7072.PreviousPolicyNumber = EntS7072.PreviousPolicyNumber
/*[COMMENT]*/                    /*IntS7072.DocumentUID  = EntS7072.DocumentUID */ /*kridtiya i. 03/02/2020*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.AgreeDt = EntS7072.AgreeDt
/*[EXECUTABLE]*/                 IntS7072.IssueDt = EntS7072.IssueDt
/*[EXECUTABLE]*/                 IntS7072.EffectiveDt  = EntS7072.EffectiveDt
/*[EXECUTABLE]*/                 IntS7072.ExpirationDt = EntS7072.ExpirationDt
/*[EXECUTABLE]*/                 IntS7072.EndDt   = EntS7072.EndDt
/*[EXECUTABLE]*/                 IntS7072.SetTime = EntS7072.SetTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.TPBIAmtPerson   = EntS7072.TPBIAmtPerson
/*[EXECUTABLE]*/                 IntS7072.TPBIAmtAccident = EntS7072.TPBIAmtAccident
/*[EXECUTABLE]*/                 IntS7072.PDAmtAccident   = EntS7072.PDAmtAccident
/*[EXECUTABLE]*/                 IntS7072.DeductiblePDAmtAccident = EntS7072.DeductiblePDAmtAccident
/*[EXECUTABLE]*/                 IntS7072.COLLAmtAccident           = EntS7072.COLLAmtAccident
/*[EXECUTABLE]*/                 IntS7072.DeductibleCOLLAmtAccident = EntS7072.DeductibleCOLLAmtAccident
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.FTAmt = EntS7072.FTAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.PerilsPADriverAmt = EntS7072.PerilsPADriverAmt
/*[EXECUTABLE]*/                 IntS7072.PerilsPANumPassengers = EntS7072.PerilsPANumPassengers
/*[EXECUTABLE]*/                 IntS7072.PerilsPAPassengerAmt  = EntS7072.PerilsPAPassengerAmt
/*[EXECUTABLE]*/                 IntS7072.PerilsPATemporaryDriverAmt  = EntS7072.PerilsPATemporaryDriverAmt
/*[EXECUTABLE]*/                 IntS7072.PerilsPANumTemporaryPassengers = EntS7072.PerilsPANumTemporaryPassengers
/*[EXECUTABLE]*/                 IntS7072.PerilsPATemporaryPassengerAmt  = EntS7072.PerilsPATemporaryPassengerAmt
/*[EXECUTABLE]*/                 IntS7072.PerilsMedicalTreatmentAmt  = EntS7072.PerilsMedicalTreatmentAmt
/*[EXECUTABLE]*/                 IntS7072.PerilsBailBondInsuranceAmt = EntS7072.PerilsBailBondInsuranceAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.DiscountForNamedDriver = EntS7072.DiscountForNamedDriver
/*[EXECUTABLE]*/                 IntS7072.DeductibleAmt = EntS7072.DeductibleAmt
/*[EXECUTABLE]*/                 IntS7072.FleetAmt      = EntS7072.FleetAmt
/*[EXECUTABLE]*/                 IntS7072.GoodDriverIndPct = EntS7072.GoodDriverIndPct
/*[EXECUTABLE]*/                 IntS7072.GoodDriverIndAmt = EntS7072.GoodDriverIndAmt
/*[EXECUTABLE]*/                 IntS7072.OtherDiscountAmt = EntS7072.OtherDiscountAmt
/*[EXECUTABLE]*/                 IntS7072.TotalDiscountsAmt  = EntS7072.TotalDiscountsAmt
/*[EXECUTABLE]*/                 IntS7072.SurchargeFactorAmt = EntS7072.SurchargeFactorAmt
/*[EXECUTABLE]*/                 IntS7072.PremiumCoverage13Amt = EntS7072.PremiumCoverage13Amt
/*[EXECUTABLE]*/                 IntS7072.PremiumCoverage2Amt  = EntS7072.PremiumCoverage2Amt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.ReceiptNumber   = EntS7072.ReceiptNumber /* CampaignNumber / PromotionNumber */
/*[EXECUTABLE]*/                 IntS7072.WrittenAmt      = EntS7072.WrittenAmt
/*[EXECUTABLE]*/                 IntS7072.RevenueStampAmt = EntS7072.RevenueStampAmt
/*[EXECUTABLE]*/                 IntS7072.VatAmt          = EntS7072.VatAmt
/*[EXECUTABLE]*/                 IntS7072.CurrentTermAmt  = EntS7072.CurrentTermAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.GarageTypeCd    = EntS7072.GarageTypeCd
/*[EXECUTABLE]*/                 IntS7072.GarageDesc      = EntS7072.GarageDesc
/*[EXECUTABLE]*/                 IntS7072.OptionValueDesc = EntS7072.OptionValueDesc .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.CMIPolicyTypeCd = EntS7072.CMIPolicyTypeCd 
/*[EXECUTABLE]*/                 IntS7072.CMIVehTypeCd    = EntS7072.CMIVehTypeCd
/*[EXECUTABLE]*/                 IntS7072.CMIPolicyNumber = EntS7072.CMIPolicyNumber
/*[EXECUTABLE]*/                 IntS7072.CMIApplicationNumber = EntS7072.CMIApplicationNumber
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    IntS7072.CMIBarCodeNumber = EntS7072.CMIBarCodeNumber
/*[EXECUTABLE]*/                 IntS7072.CMIDocumentUID   = EntS7072.CMIDocumentUID*/  /*kridtiya i. 03/02/2020*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.CMIEffectiveDt  = EntS7072.CMIEffectiveDt
/*[EXECUTABLE]*/                 IntS7072.CMIExpirationDt = EntS7072.CMIExpirationDt
/*[EXECUTABLE]*/                 IntS7072.CMIAmtPerson    = EntS7072.CMIAmtPerson
/*[EXECUTABLE]*/                 IntS7072.CMIAmtAccident  = EntS7072.CMIAmtAccident
/*[EXECUTABLE]*/                 IntS7072.CMIWrittenAmt   = EntS7072.CMIWrittenAmt
/*[EXECUTABLE]*/                 IntS7072.CMIRevenueStampAmt = EntS7072.CMIRevenueStampAmt
/*[EXECUTABLE]*/                 IntS7072.CMIVatAmt          = EntS7072.CMIVatAmt
/*[EXECUTABLE]*/                 IntS7072.CMICurrentTermAmt  = EntS7072.CMICurrentTermAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.MsgStatusCd    = EntS7072.MsgStatusCd
/*[EXECUTABLE]*/                 IntS7072.AgencyEmployee = EntS7072.AgencyEmployee 
/*[EXECUTABLE]*/                 IntS7072.RemarkText     = EntS7072.RemarkText
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.SystemRq = EntS7072.SystemRq
/*[EXECUTABLE]*/                 IntS7072.RqUID    = EntS7072.RqUID
/*[EXECUTABLE]*/                 IntS7072.keyRequestIndRq = EntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                 IntS7072.Policy = EntS7072.Policy
/*[EXECUTABLE]*/                 IntS7072.Rencnt = EntS7072.Rencnt
/*[EXECUTABLE]*/                 IntS7072.Endcnt = EntS7072.Endcnt
/*[EXECUTABLE]*/                 IntS7072.Riskno = EntS7072.Riskno
/*[EXECUTABLE]*/                 IntS7072.Itemno = EntS7072.Itemno
/*[EXECUTABLE]*/                 IntS7072.vehreg = EntS7072.vehreg
/*[EXECUTABLE]*/                 IntS7072.accdat = EntS7072.accdat
/*[EXECUTABLE]*/                 IntS7072.comdat = EntS7072.comdat
/*[EXECUTABLE]*/                 IntS7072.expdat = EntS7072.expdat
/*[EXECUTABLE]*/                 IntS7072.CMIComDate    = EntS7072.CMIComDate
/*[EXECUTABLE]*/                 IntS7072.CMIExpDate    = EntS7072.CMIExpDate
/*[EXECUTABLE]*/                 IntS7072.BirthDate     = EntS7072.BirthDate
/*[EXECUTABLE]*/                 IntS7072.InsUIDExpDate = EntS7072.InsUIDExpDate
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.ReceiptName2 = EntS7072.ReceiptName2
/*[EXECUTABLE]*/                 IntS7072.ReceiptAddr2 = EntS7072.ReceiptAddr2
/*[EXECUTABLE]*/                 IntS7072.TransactionDateRq = EntS7072.TransactionDateRq
/*[EXECUTABLE]*/                 IntS7072.TransactionTimeRq = EntS7072.TransactionTimeRq
/*[EXECUTABLE]*/                 IntS7072.BirthDate1 = EntS7072.BirthDate1
/*[EXECUTABLE]*/                 IntS7072.BirthDate2 = EntS7072.BirthDate2
/*[EXECUTABLE]*/                 IntS7072.InsUIDExpDate1 = EntS7072.InsUIDExpDate1
/*[EXECUTABLE]*/                 IntS7072.InsUIDExpDate2 = EntS7072.InsUIDExpDate2
/*[EXECUTABLE]*/                 IntS7072.RegisterDt = EntS7072.RegisterDt
/*[EXECUTABLE]*/                 IntS7072.ShowroomID = EntS7072.ShowroomID
/*[EXECUTABLE]*/                 IntS7072.ShowroomName = EntS7072.ShowroomName
/*[EXECUTABLE]*/                 IntS7072.ReceiptDt  = EntS7072.ReceiptDt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.AgentBrokerLicenseNumber = nv_Acno1 /*EntS7072.AgentBrokerLicenseNumber*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /*02/10/2015*/
/*[EXECUTABLE]*/                 IntS7072.Statusflag = EntS7072.Statusflag  /*Flag Ã¶·Ñ¹ã¨ à©¾ÒÐ»ÃÐàÀ· 1*/
/*[EXECUTABLE]*/                 IntS7072.Finint     = EntS7072.Finint /*Deler / VAT CODE*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.TransferToPremium = NO .
/*[COMMENT]*/                    /* --------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN PD_ChkDataExt2. /*Save ¢éÍÁÙÅÊèÇ¹·ÕèàËÅ×Í*/
/*[EXECUTABLE]*/                 RUN PD_ChkDataExt3. /*Save ¢éÍÁÙÅÊèÇ¹·ÕèàËÅ×Í*/
/*[COMMENT]*/                    /* --------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF IntS7072.PolicyStatus   = "" THEN IntS7072.PolicyStatus   = "N".
/*[EXECUTABLE]*/                 IF IntS7072.DocumentUID    = "" THEN IntS7072.DocumentUID    = nv_Mdocno1.
/*[EXECUTABLE]*/                 IF IntS7072.CMIDocumentUID = "" THEN IntS7072.CMIDocumentUID = nv_Tdocno1.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF IntS7072.CMIVehTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                   FIND FIRST FMSclassCST WHERE FMSclassCST.Sclass = IntS7072.CMIVehTypeCd
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE FMSclassCST THEN DO:
/*[EXECUTABLE]*/                     IF FMSclassCST.CST = "C" THEN DO:
/*[EXECUTABLE]*/                       IF IntS7072.Displacement = "" THEN DO:
/*[EXECUTABLE]*/                         IF FMSclassCST.MaxCST <> 0 THEN 
/*[EXECUTABLE]*/                           IntS7072.Displacement = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF FMSclassCST.CST = "S" THEN DO:
/*[EXECUTABLE]*/                       IF IntS7072.SeatingCapacity = "" THEN DO:
/*[EXECUTABLE]*/                         IF FMSclassCST.MaxCST <> 0 THEN 
/*[EXECUTABLE]*/                           IntS7072.SeatingCapacity = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF FMSclassCST.CST = "T" THEN DO:
/*[EXECUTABLE]*/                       IF IntS7072.GrossVehOrCombinedWeight = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF FMSclassCST.MaxCST <> 0 THEN 
/*[EXECUTABLE]*/                           IntS7072.GrossVehOrCombinedWeight = TRIM(STRING(FMSclassCST.MaxCST,">>>>9")).
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 IF LENGTH(TRIM(IntS7072.RegisteredProvCd)) > 2 THEN DO:
/*[EXECUTABLE]*/                   FOR EACH uwm500 USE-INDEX uwm50002 NO-LOCK:
/*[EXECUTABLE]*/                     IF INDEX(uwm500.prov_d,IntS7072.RegisteredProvCd) <> 0 THEN DO:
/*[EXECUTABLE]*/                       IntS7072.RegisteredProvCd = uwm500.prov_n.
/*[EXECUTABLE]*/                       LEAVE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF CAN-DO(uwm500.prov_d,IntS7072.RegisteredProvCd) THEN DO:
/*[EXECUTABLE]*/                       IntS7072.RegisteredProvCd = uwm500.prov_n.
/*[EXECUTABLE]*/                       LEAVE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 IntS7072.RegisteredProvinceCode = IntS7072.RegisteredProvCd.
/*[EXECUTABLE]*/                 IF EntS7072.CompanyCode = "469" THEN IntS7072.vehreg = TRIM(IntS7072.Registration). /*kridtiyai.*/
/*[EXECUTABLE]*/                 ELSE IntS7072.vehreg = TRIM(IntS7072.Registration) + " " + TRIM(IntS7072.RegisteredProvCd).
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.ResultStatus     = ""   /*EntS7072.ResultStatus*/
/*[EXECUTABLE]*/                 IntS7072.ProcessStatus    = "O"
/*[EXECUTABLE]*/                 IntS7072.ProcessByUser    = EntS7072.ProcessByUser /*"Queue"*/
/*[EXECUTABLE]*/                 IntS7072.ProcessDate      = TODAY
/*[EXECUTABLE]*/                 IntS7072.ProcessTime      = STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                 IntS7072.TrnFromIntByUser = EntS7072.TrnFromIntByUser /*"Queue"*/
/*[EXECUTABLE]*/                 IntS7072.TrnFromIntDate   = TODAY
/*[EXECUTABLE]*/                 IntS7072.TrnFromIntTime   = STRING(TIME,"HH:MM:SS")
/*[COMMENT]*/                    /*yyyymmddhhmmss999*/
/*[EXECUTABLE]*/                 IntS7072.ReferenceNumber  = STRING( YEAR(TODAY),"9999")
/*[EXECUTABLE]*/                                           + STRING(MONTH(TODAY),"99")
/*[EXECUTABLE]*/                                           + STRING(  DAY(TODAY),"99")
/*[EXECUTABLE]*/                                           + SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12).
/*[EXECUTABLE]*/                 IntS7072.ReferenceNumber  = REPLACE(IntS7072.ReferenceNumber,":","").
/*[EXECUTABLE]*/                 IntS7072.ReferenceNumber  = REPLACE(IntS7072.ReferenceNumber,".","").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.ReceiveNumber = IntS7072.ReferenceNumber
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÃÐºØµÒÁ USER ID ·ÕèConfirm data ¡èÍ¹Êè§¡ÅÑº ä»ãËé REQUESTOR */
/*[EXECUTABLE]*/                 IntS7072.ConfirmBy     = nv_ConfirmBy
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 EntS7072.ProcessStatus = "O".
/*[COMMENT]*/                    /* --------------------------------------------------------------------------- */
/*[COMMENT]*/                    /*µÃÇ¨ÊÍº¢éÍÁÙÅ â´Âà¨éÒË¹éÒ·Õè »ÃÐàÀ· 1,2,2.1,3,3.1*/
/*[EXECUTABLE]*/                 nv_PolicyTypeCd = "".
/*[EXECUTABLE]*/                 IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN
/*[EXECUTABLE]*/                   nv_PolicyTypeCd = IntS7072.PolicyTypeCd.
/*[EXECUTABLE]*/                 ELSE DO:
/*[EXECUTABLE]*/                   IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_PolicyTypeCd = IntS7072.CMIPolicyTypeCd.
/*[EXECUTABLE]*/                     FOR EACH msgcode WHERE
/*[EXECUTABLE]*/                              MsgCode.CompNo = IntS7072.CompanyCode
/*[EXECUTABLE]*/                          AND MsgCode.MsgNo  = "GrpClass"
/*[EXECUTABLE]*/                     NO-LOCK:
/*[EXECUTABLE]*/                       IF msgcode.Branch = IntS7072.CMIVehTypeCd AND MsgCode.MsgDesc = "NOTSALE" THEN DO:
/*[EXECUTABLE]*/                         IntS7072.ProcessStatus = "E".
/*[EXECUTABLE]*/                         EntS7072.ProcessStatus = "E".
/*[EXECUTABLE]*/                         RUN PD_SaveError ("äÁè¾ºÃËÑÊ¾Ãº. " + IntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ").
/*[EXECUTABLE]*/                         RETURN.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 FIND FIRST FModify WHERE
/*[EXECUTABLE]*/                            FModify.SystemName  = IntS7072.SystemRq
/*[EXECUTABLE]*/                      AND   FModify.CompanyCode = IntS7072.CompanyCode
/*[EXECUTABLE]*/                      AND   FModify.CoverTypeCd = nv_PolicyTypeCd 
/*[EXECUTABLE]*/                 NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE FModify THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF FModify.AutoAll = YES THEN DO:   /*ãËé¼èÒ¹ä´é·Ñé§ËÁ´*/
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     IntS7072.ConfirmBy        = "AUTO"
/*[EXECUTABLE]*/                     IntS7072.ByUserID         = FModify.ChkVehAssignBy
/*[EXECUTABLE]*/                     IntS7072.ChkVehicle       = NO
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignBy   = FModify.ChkVehAssignBy
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignDt   = TODAY
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignTime = STRING(TIME,"HH:MM:SS").
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[COMMENT]*/                        /*µÃÇ¨ÊÀÒ¾Ã¶ â´Âà¨éÒË¹éÒ·Õè »ÃÐàÀ· 1,2,2.1*/
/*[EXECUTABLE]*/                     IF FModify.ChkVehicle = NO THEN 
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       IntS7072.ConfirmBy  = "AUTO" /*äÁèµéÍ§µÃÇ¨ÊÀÒ¾*/
/*[EXECUTABLE]*/                       IntS7072.ChkVehicle = NO.
/*[EXECUTABLE]*/                     ELSE
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       IntS7072.ConfirmBy  = "NO"               /*Êè§ µÃÇ¨ÊÀÒ¾Ã¶*/
/*[EXECUTABLE]*/                       IntS7072.ChkVehicle = FModify.ChkVehicle /*YES*/
/*[EXECUTABLE]*/                       IntS7072.ChkVehDt   = TODAY
/*[EXECUTABLE]*/                       IntS7072.ChkVehTime = STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                       IntS7072.ChkVehBy   = FModify.AssignTO         /*ª×èÍà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÀÒ¾Ã¶*/
/*[EXECUTABLE]*/                       IntS7072.ChkVehSend = FModify.AssignSendToMail /*Y/N Êè§mailãËéà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÍºÊÀÒ¾Ã¶ËÃ×ÍäÁè */
/*[EXECUTABLE]*/                       IntS7072.ChkVehMail = FModify.AssignSendMail   /*mail à¨éÒË¹éÒ·ÕèµÃÇ¨ÊÀÒ¾Ã¶*/
/*[EXECUTABLE]*/                       IntS7072.ChkVehAssignSend = FModify.ChkVehAssignSendToMail /*Y/N Êè§mailËÃ×ÍäÁè */
/*[EXECUTABLE]*/                       IntS7072.ChkVehAssignMail = FModify.ChkVehAssignMail       /*mail á¨é§ user µÃÇ¨ÊÍº¢éÍÁÙÅ*/
/*[COMMENT]*/                          /* »ÃÐàÀ· 1,2,2.1 */
/*[COMMENT]*/                          /* FModify.TransferToPremium = YES ãËéâÍ¹¢éÍÁÙÅà¢éÒÃÐºº Premium*/
/*[COMMENT]*/                          /* ÊÓËÃÑºà»ç¹¢éÍÁÙÅã¹¡ÒÃÊè§µÃÇ¨ÊÀÒ¾Ã¶ áÅÐ¶éÒ¼èÒ¹ÊÒÁÒÃ¶·Ó§Ò¹ä´é·Ñ¹·Õ*/
/*[EXECUTABLE]*/                       IntS7072.TransferToPremium = FModify.TransferToPremium . 
/*[COMMENT]*/                        /* -------------------------------------------------- */
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     IntS7072.ByUserID          = FModify.ChkVehAssignBy
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignBy    = FModify.ChkVehAssignBy /*ª×èÍà¨éÒË¹éÒ·ÕèµÃÇ¨ÊÍº¢éÍÁÙÅ*/
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignDt    = TODAY
/*[EXECUTABLE]*/                     IntS7072.ChkVehAssignTime  = STRING(TIME,"HH:MM:SS").
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO ChkVeh_PD_Ext1.txt APPEND.
/*[EXECUTABLE]*/                   PUT "PD_ChkDataExt1 : " TODAY FORMAT "99/99/9999" STRING(TIME,"HH:MM:SS") SKIP.
/*[EXECUTABLE]*/                   PUT "-----------------------------------------------------------" FORMAT "x(80)" SKIP.
/*[EXECUTABLE]*/                   PUT "ContractNumber:" IntS7072.ContractNumber SKIP.
/*[EXECUTABLE]*/                   PUT "PolicyTypeCd:" IntS7072.PolicyTypeCd FORMAT "X(5)" SKIP. 
/*[EXECUTABLE]*/                   PUT "RateGroup  :" IntS7072.RateGroup   SKIP.
/*[EXECUTABLE]*/                   PUT "DocumentUID:" IntS7072.DocumentUID SKIP. 
/*[EXECUTABLE]*/                   PUT "EntS7072.RegisteredProvCd:" EntS7072.RegisteredProvCd FORMAT "X(15)" SKIP.
/*[EXECUTABLE]*/                   PUT "IntS7072.RegisteredProvCd:" IntS7072.RegisteredProvCd FORMAT "X(15)" SKIP.
/*[EXECUTABLE]*/                   PUT "Registration :" IntS7072.Registration FORMAT "X(15)" SKIP.
/*[EXECUTABLE]*/                   PUT "CMIPolicyTypeCd:" IntS7072.CMIPolicyTypeCd FORMAT "X(5)" SKIP.
/*[EXECUTABLE]*/                   PUT "CMIVehTypeCd  :" IntS7072.CMIVehTypeCd   SKIP.
/*[EXECUTABLE]*/                   PUT "CMIDocumentUID:" IntS7072.CMIDocumentUID SKIP.
/*[EXECUTABLE]*/                   PUT "ConfirmBy :" IntS7072.ConfirmBy  SKIP.
/*[EXECUTABLE]*/                   PUT "ByUserID  :" IntS7072.ByUserID   SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehicle:" IntS7072.ChkVehicle SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehBy  :" IntS7072.ChkVehBy   SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehDt  :" IntS7072.ChkVehDt   SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehTime:" IntS7072.ChkVehTime SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehSend:" IntS7072.ChkVehSend SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehMail:" IntS7072.ChkVehMail SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehAssignBy:" IntS7072.ChkVehAssignBy SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehAssignDt:" IntS7072.ChkVehAssignDt SKIP.
/*[EXECUTABLE]*/                   PUT "ChkVehAssignTime :" IntS7072.ChkVehAssignTime SKIP.
/*[EXECUTABLE]*/                   PUT "TransferToPremium:" IntS7072.TransferToPremium SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                   IntS7072.ConfirmBy = "AUTO"
/*[EXECUTABLE]*/                   IntS7072.ByUserID  = "AUTObyApplication".
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 RELEASE IntS7072.
/*[EXECUTABLE]*/                 RELEASE EntS7072.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt2 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ChkDataExt2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.ResponseResult     = EntS7072.ResponseResult
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.EndorseEffectiveDt = EntS7072.EndorseEffectiveDt
/*[EXECUTABLE]*/                 IntS7072.EndorseRefNumber   = EntS7072.EndorseRefNumber
/*[EXECUTABLE]*/                 IntS7072.EndorseFlag        = EntS7072.EndorseFlag
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.InsurerCode        = EntS7072.InsurerCode
/*[EXECUTABLE]*/                 IntS7072.MethodCode         = EntS7072.MethodCode
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.SendByUser         = EntS7072.SendByUser
/*[EXECUTABLE]*/                 IntS7072.SendDate           = EntS7072.SendDate
/*[EXECUTABLE]*/                 IntS7072.SendTime           = EntS7072.SendTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.SystemErrorStatus1 = EntS7072.SystemErrorStatus1
/*[EXECUTABLE]*/                 IntS7072.SystemErrorStatus2 = EntS7072.SystemErrorStatus2
/*[EXECUTABLE]*/                 IntS7072.SystemErrorStatus3 = EntS7072.SystemErrorStatus3
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.EndorseReceiveNumber = EntS7072.EndorseReceiveNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.ErrorMessage         = EntS7072.ErrorMessage
/*[EXECUTABLE]*/                 IntS7072.ByUserID             = EntS7072.ByUserID
/*[EXECUTABLE]*/                 IntS7072.StickerNumber        = EntS7072.StickerNumber.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd," ","").
/*[EXECUTABLE]*/                 IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd,".","").
/*[EXECUTABLE]*/                 IntS7072.CMIVehTypeCd  = REPLACE(IntS7072.CMIVehTypeCd,"-","").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.District      = REPLACE(IntS7072.District,     CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(13),"").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.District      = REPLACE(IntS7072.District,     CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(10),"").
/*[COMMENT]*/                    /*add by kridtiya i. 03/04/2017*/
/*[EXECUTABLE]*/                 IntS7072.Addr          = REPLACE(IntS7072.Addr,         CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.UnitNumber    = REPLACE(IntS7072.UnitNumber,   CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.RoomNumber    = REPLACE(IntS7072.RoomNumber,   CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.Building      = REPLACE(IntS7072.Building,     CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.VillageNumber = REPLACE(IntS7072.VillageNumber,CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.Alley         = REPLACE(IntS7072.Alley,        CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.Lane          = REPLACE(IntS7072.Lane,         CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.StreetName    = REPLACE(IntS7072.StreetName,   CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.SubDistrict   = REPLACE(IntS7072.SubDistrict,  CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.District      = REPLACE(IntS7072.District,     CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.StateProvCd   = REPLACE(IntS7072.StateProvCd,  CHR(26),"").
/*[EXECUTABLE]*/                 IntS7072.StateProv     = REPLACE(IntS7072.StateProv,    CHR(26),"").
/*[COMMENT]*/                    /*add by kridtiya i. 03/04/2017*/
/*[EXECUTABLE]*/                 IntS7072.ReceiptName   = REPLACE(IntS7072.ReceiptName,  CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.ReceiptAddr   = REPLACE(IntS7072.ReceiptAddr,  CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.Beneficiaries = REPLACE(IntS7072.Beneficiaries,CHR(13),"").
/*[EXECUTABLE]*/                 IntS7072.ReceiptName   = REPLACE(IntS7072.ReceiptName,  CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.ReceiptAddr   = REPLACE(IntS7072.ReceiptAddr,  CHR(10),"").
/*[EXECUTABLE]*/                 IntS7072.Beneficiaries = REPLACE(IntS7072.Beneficiaries,CHR(10),"").
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.Registration  = REPLACE(IntS7072.Registration,"-"," ").
/*[EXECUTABLE]*/                 IntS7072.Registration  = REPLACE(IntS7072.Registration,"  "," ").
/*[EXECUTABLE]*/                 IntS7072.PlateNumber   = IntS7072.Registration.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntS7072.COUNTER_NO                = EntS7072.COUNTER_NO
/*[EXECUTABLE]*/                 IntS7072.TERM_NO                   = EntS7072.TERM_NO
/*[EXECUTABLE]*/                 IntS7072.SERVICE_RUN_NO            = EntS7072.SERVICE_RUN_NO
/*[EXECUTABLE]*/                 IntS7072.RECORD_STATUS             = EntS7072.RECORD_STATUS
/*[EXECUTABLE]*/                 IntS7072.CLIENT_SERVICE_RUNNO      = EntS7072.CLIENT_SERVICE_RUNNO
/*[EXECUTABLE]*/                 IntS7072.ZONE                      = EntS7072.ZONE
/*[EXECUTABLE]*/                 IntS7072.R_SERVICE_RUNN            = EntS7072.R_SERVICE_RUNN
/*[EXECUTABLE]*/                 IntS7072.CANCEL_OPERATING          = EntS7072.CANCEL_OPERATING
/*[EXECUTABLE]*/                 IntS7072.OPERATE_BY_STAFF          = EntS7072.OPERATE_BY_STAFF
/*[EXECUTABLE]*/                 IntS7072.SYSTEM_DATE_TIME          = EntS7072.SYSTEM_DATE_TIME
/*[EXECUTABLE]*/                 IntS7072.USERID_CS                 = EntS7072.USERID_CS
/*[EXECUTABLE]*/                 IntS7072.PASSWORD_CS               = EntS7072.PASSWORD_CS
/*[EXECUTABLE]*/                 IntS7072.SUCCESS                   = EntS7072.SUCCESS
/*[EXECUTABLE]*/                 IntS7072.CODE                      = EntS7072.CODE
/*[EXECUTABLE]*/                 IntS7072.DESC_CS                   = EntS7072.DESC_CS
/*[EXECUTABLE]*/                 IntS7072.METHOD                    = EntS7072.METHOD
/*[EXECUTABLE]*/                 IntS7072.TX_ID                     = EntS7072.TX_ID
/*[EXECUTABLE]*/                 IntS7072.LOG_ID                    = EntS7072.LOG_ID
/*[EXECUTABLE]*/                 IntS7072.VENDOR_ID                 = EntS7072.VENDOR_ID
/*[EXECUTABLE]*/                 IntS7072.SERVICE_ID                = EntS7072.SERVICE_ID
/*[EXECUTABLE]*/                 IntS7072.INSURER_PRODUCT_LINE_CODE = EntS7072.INSURER_PRODUCT_LINE_CODE
/*[EXECUTABLE]*/                 IntS7072.PRODUCT_CODE              = EntS7072.PRODUCT_CODE
/*[EXECUTABLE]*/                 IntS7072.PLAN_CODE                 = EntS7072.PLAN_CODE
/*[EXECUTABLE]*/                 IntS7072.CATEGORY                  = EntS7072.CATEGORY
/*[EXECUTABLE]*/                 IntS7072.BILLING_FREQ              = EntS7072.BILLING_FREQ
/*[EXECUTABLE]*/                 IntS7072.NET_PREMIUM               = EntS7072.NET_PREMIUM
/*[EXECUTABLE]*/                 IntS7072.GROSS_PREMIUM             = EntS7072.GROSS_PREMIUM
/*[EXECUTABLE]*/                 IntS7072.SUM_INSURE                = EntS7072.SUM_INSURE
/*[EXECUTABLE]*/                 IntS7072.PRINT_SLIP                = EntS7072.PRINT_SLIP
/*[EXECUTABLE]*/                 IntS7072.POL_NO                    = EntS7072.POL_NO
/*[EXECUTABLE]*/                 IntS7072.CERT_NO                   = EntS7072.CERT_NO
/*[EXECUTABLE]*/                 IntS7072.OLD_POL_NO                = EntS7072.OLD_POL_NO
/*[EXECUTABLE]*/                 IntS7072.NEW_RENEW                 = EntS7072.NEW_RENEW
/*[EXECUTABLE]*/                 IntS7072.POL_YEAR_SEQ              = EntS7072.POL_YEAR_SEQ
/*[EXECUTABLE]*/                 IntS7072.SALE_DATE                 = EntS7072.SALE_DATE
/*[EXECUTABLE]*/                 IntS7072.EFFECTIVE_DATE            = EntS7072.EFFECTIVE_DATE
/*[EXECUTABLE]*/                 IntS7072.END_DATE                  = EntS7072.END_DATE
/*[EXECUTABLE]*/                 IntS7072.NID_NO                    = EntS7072.NID_NO
/*[EXECUTABLE]*/                 IntS7072.CARD_TITLE                = EntS7072.CARD_TITLE
/*[EXECUTABLE]*/                 IntS7072.CARD_NAME                 = EntS7072.CARD_NAME
/*[EXECUTABLE]*/                 IntS7072.CARD_MNAME                = EntS7072.CARD_MNAME
/*[EXECUTABLE]*/                 IntS7072.CARD_SNAME                = EntS7072.CARD_SNAME
/*[EXECUTABLE]*/                 IntS7072.CARD_DOB                  = EntS7072.CARD_DOB
/*[EXECUTABLE]*/                 IntS7072.CARD_GENDER               = EntS7072.CARD_GENDER
/*[EXECUTABLE]*/                 IntS7072.CARD_ADDRESS              = EntS7072.CARD_ADDRESS
/*[EXECUTABLE]*/                 IntS7072.CARD_SUB_DISTRICT         = EntS7072.CARD_SUB_DISTRICT
/*[EXECUTABLE]*/                 IntS7072.CARD_DISTRICT             = EntS7072.CARD_DISTRICT
/*[EXECUTABLE]*/                 IntS7072.CARD_PROVINCE             = EntS7072.CARD_PROVINCE
/*[EXECUTABLE]*/                 IntS7072.CARD_ISSUE_DATE           = EntS7072.CARD_ISSUE_DATE
/*[EXECUTABLE]*/                 IntS7072.CARD_EXPIRED_DATE         = EntS7072.CARD_EXPIRED_DATE
/*[EXECUTABLE]*/                 IntS7072.TEL_NO                    = EntS7072.TEL_NO
/*[EXECUTABLE]*/                 IntS7072.CURRENT_ADDRESS           = EntS7072.CURRENT_ADDRESS
/*[EXECUTABLE]*/                 IntS7072.CURRENT_SUB_DISTRICT      = EntS7072.CURRENT_SUB_DISTRICT
/*[EXECUTABLE]*/                 IntS7072.CURRENT_DISTRICT          = EntS7072.CURRENT_DISTRICT
/*[EXECUTABLE]*/                 IntS7072.CURRENT_PROVINCE          = EntS7072.CURRENT_PROVINCE
/*[EXECUTABLE]*/                 IntS7072.PAYCODE                   = EntS7072.PAYCODE
/*[EXECUTABLE]*/                 IntS7072.CHASSIS_NO                = EntS7072.CHASSIS_NO
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_REG_DATE          = EntS7072.VEHICLE_REG_DATE
/*[EXECUTABLE]*/                 IntS7072.CAR_REG_NO                = EntS7072.CAR_REG_NO
/*[EXECUTABLE]*/                 IntS7072.CAR_REG_PROVINCE          = EntS7072.CAR_REG_PROVINCE
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_TYPE_CODE         = EntS7072.VEHICLE_TYPE_CODE
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_BODY_TYPE         = EntS7072.VEHICLE_BODY_TYPE
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_MAKE              = EntS7072.VEHICLE_MAKE
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_MODEL             = EntS7072.VEHICLE_MODEL
/*[EXECUTABLE]*/                 IntS7072.VEHICLE_USE_CODE          = EntS7072.VEHICLE_USE_CODE
/*[EXECUTABLE]*/                 IntS7072.ENGINE_CC                 = EntS7072.ENGINE_CC
/*[EXECUTABLE]*/                 IntS7072.USE_AREA                  = EntS7072.USE_AREA
/*[EXECUTABLE]*/                 IntS7072.DRIVER_TITLE_1            = EntS7072.DRIVER_TITLE_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_NAME_1             = EntS7072.DRIVER_NAME_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_SURNAME_1          = EntS7072.DRIVER_SURNAME_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_GENDER_1           = EntS7072.DRIVER_GENDER_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_AGE_1              = EntS7072.DRIVER_AGE_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_LICENSE_1          = EntS7072.DRIVER_LICENSE_1
/*[EXECUTABLE]*/                 IntS7072.DRIVER_TITLE_2            = EntS7072.DRIVER_TITLE_2
/*[EXECUTABLE]*/                 IntS7072.DRIVER_NMAE_2             = EntS7072.DRIVER_NMAE_2
/*[EXECUTABLE]*/                 IntS7072.DRIVER_SURNAME_2          = EntS7072.DRIVER_SURNAME_2
/*[EXECUTABLE]*/                 IntS7072.DRIVER_GENDER_2           = EntS7072.DRIVER_GENDER_2
/*[EXECUTABLE]*/                 IntS7072.DRIVER_AGE_2              = EntS7072.DRIVER_AGE_2
/*[EXECUTABLE]*/                 IntS7072.DRIVER_LICENSE_2          = EntS7072.DRIVER_LICENSE_2
/*[EXECUTABLE]*/                 IntS7072.COMP_BARCODE              = EntS7072.COMP_BARCODE
/*[EXECUTABLE]*/                 IntS7072.RECEIVE_NO                = EntS7072.RECEIVE_NO
/*[EXECUTABLE]*/                 IntS7072.INVOICE_NO                = EntS7072.INVOICE_NO
/*[EXECUTABLE]*/                 IntS7072.STAMP_RATE                = EntS7072.STAMP_RATE
/*[EXECUTABLE]*/                 IntS7072.VAT                       = EntS7072.VAT .
/*[COMMENT]*/                    /* ------ */
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ChkDataExt3 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ChkDataExt3 :
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
/*[EXECUTABLE]*/                 IF LENGTH(TRIM(EntS7072.RegisteredProvCd)) > 2 THEN DO:
/*[EXECUTABLE]*/                     IF      index(EntS7072.RegisteredProvCd,"¡·")              <> 0 THEN IntS7072.RegisteredProvCd = "¡·".  
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃØ§à·¾")         <> 0 THEN IntS7072.RegisteredProvCd = "¡·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃØ§à·¾ÁËÒ¹¤Ã")   <> 0 THEN IntS7072.RegisteredProvCd = "¡·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡ÃÐºÕè")          <> 0 THEN IntS7072.RegisteredProvCd = "¡º".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡Ò­¨¹ºØÃÕ")       <> 0 THEN IntS7072.RegisteredProvCd = "¡¨".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡ÒÌÊÔ¹¸Øì")       <> 0 THEN IntS7072.RegisteredProvCd = "¡Ê".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¡Óá¾§à¾ªÃ")       <> 0 THEN IntS7072.RegisteredProvCd = "¡¾".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¢Í¹á¡è¹")         <> 0 THEN IntS7072.RegisteredProvCd = "¢¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¨Ñ¹·ºØÃÕ")        <> 0 THEN IntS7072.RegisteredProvCd = "¨º".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"©ÐàªÔ§à·ÃÒ")      <> 0 THEN IntS7072.RegisteredProvCd = "©ª".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ªÅºØÃÕ")          <> 0 THEN IntS7072.RegisteredProvCd = "ªº".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ªÑÂ¹Ò·")          <> 0 THEN IntS7072.RegisteredProvCd = "ª¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ªÑÂÀÙÁÔ")         <> 0 THEN IntS7072.RegisteredProvCd = "ªÂ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ªØÁ¾Ã")           <> 0 THEN IntS7072.RegisteredProvCd = "ª¾".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"àªÕÂ§ÃÒÂ")        <> 0 THEN IntS7072.RegisteredProvCd = "ªÃ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"àªÕÂ§ãËÁè")       <> 0 THEN IntS7072.RegisteredProvCd = "ªÁ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"µÃÑ§")            <> 0 THEN IntS7072.RegisteredProvCd = "µ§".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"µÃÒ´")            <> 0 THEN IntS7072.RegisteredProvCd = "µÃ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"µÒ¡")             <> 0 THEN IntS7072.RegisteredProvCd = "µ¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã¹ÒÂ¡")         <> 0 THEN IntS7072.RegisteredProvCd = "¹Â".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã»°Á")          <> 0 THEN IntS7072.RegisteredProvCd = "¹°".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤Ã¾¹Á")          <> 0 THEN IntS7072.RegisteredProvCd = "¹¾".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÃÒªÊÕÁÒ")      <> 0 THEN IntS7072.RegisteredProvCd = "¹Á".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÈÃÕ¸ÃÃÁÃÒª")   <> 0 THEN IntS7072.RegisteredProvCd = "¹È".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¤ÃÊÇÃÃ¤ì")       <> 0 THEN IntS7072.RegisteredProvCd = "¹Ç".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹¹·ºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "¹º".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹ÃÒ¸ÔÇÒÊ")        <> 0 THEN IntS7072.RegisteredProvCd = "¹¸".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¹èÒ¹")            <> 0 THEN IntS7072.RegisteredProvCd = "¹¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ºÕ§¡ÒÌ")          <> 0 THEN IntS7072.RegisteredProvCd = "º¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ºØÃÕÃÑÁÂì")       <> 0 THEN IntS7072.RegisteredProvCd = "ºÃ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"»·ØÁ¸Ò¹Õ")        <> 0 THEN IntS7072.RegisteredProvCd = "»·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"»ÃÐ¨Çº¤ÕÃÕ¢Ñ¹¸ì") <> 0 THEN IntS7072.RegisteredProvCd = "»¢".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"»ÃÒ¨Õ¹ºØÃÕ")      <> 0 THEN IntS7072.RegisteredProvCd = "»¨".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"»ÑµµÒ¹Õ")         <> 0 THEN IntS7072.RegisteredProvCd = "»¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾ÃÐ¹¤ÃÈÃÕÍÂØ¸ÂÒ") <> 0 THEN IntS7072.RegisteredProvCd = "ÍÂ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾ÐàÂÒ")           <> 0 THEN IntS7072.RegisteredProvCd = "¾Â".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾Ñ§§Ò")           <> 0 THEN IntS7072.RegisteredProvCd = "¾§".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾Ñ·ÅØ§")          <> 0 THEN IntS7072.RegisteredProvCd = "¾·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾Ô¨ÔµÃ")          <> 0 THEN IntS7072.RegisteredProvCd = "¾¨".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"¾ÔÉ³ØâÅ¡")        <> 0 THEN IntS7072.RegisteredProvCd = "¾Å".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"à¾ªÃºØÃÕ")        <> 0 THEN IntS7072.RegisteredProvCd = "¾º".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"à¾ªÃºÙÃ³ì")       <> 0 THEN IntS7072.RegisteredProvCd = "¾ª".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"á¾Ãè")            <> 0 THEN IntS7072.RegisteredProvCd = "¾Ã".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÀÙà¡çµ")          <> 0 THEN IntS7072.RegisteredProvCd = "À¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÁËÒÊÒÃ¤ÒÁ")       <> 0 THEN IntS7072.RegisteredProvCd = "Á¤".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÁØ¡´ÒËÒÃ")        <> 0 THEN IntS7072.RegisteredProvCd = "ÁË".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"áÁèÎèÍ§ÊÍ¹")      <> 0 THEN IntS7072.RegisteredProvCd = "ÁÊ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÂâÊ¸Ã")           <> 0 THEN IntS7072.RegisteredProvCd = "ÂÊ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÂÐÅÒ")            <> 0 THEN IntS7072.RegisteredProvCd = "ÂÅ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÃéÍÂàÍç´")        <> 0 THEN IntS7072.RegisteredProvCd = "ÃÍ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÃÐ¹Í§")           <> 0 THEN IntS7072.RegisteredProvCd = "Ã¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÃÐÂÍ§")           <> 0 THEN IntS7072.RegisteredProvCd = "ÃÂ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÃÒªºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ãº".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"Å¾ºØÃÕ")          <> 0 THEN IntS7072.RegisteredProvCd = "Åº".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÅÓ»Ò§")           <> 0 THEN IntS7072.RegisteredProvCd = "Å»".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÅÓ¾Ù¹")           <> 0 THEN IntS7072.RegisteredProvCd = "Å¾".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"àÅÂ")             <> 0 THEN IntS7072.RegisteredProvCd = "ÅÂ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÈÃÕÊÐà¡É")        <> 0 THEN IntS7072.RegisteredProvCd = "È¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"Ê¡Å¹¤Ã")          <> 0 THEN IntS7072.RegisteredProvCd = "Ê¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"Ê§¢ÅÒ")           <> 0 THEN IntS7072.RegisteredProvCd = "Ê¢".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊµÙÅ")            <> 0 THEN IntS7072.RegisteredProvCd = "Êµ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·Ã»ÃÒ¡ÒÃ")     <> 0 THEN IntS7072.RegisteredProvCd = "Ê»".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·ÃÊ§¤ÃÒÁ")     <> 0 THEN IntS7072.RegisteredProvCd = "ÊÊ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÁØ·ÃÊÒ¤Ã")       <> 0 THEN IntS7072.RegisteredProvCd = "Ê¤".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÃÐá¡éÇ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ê¡".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÃÐºØÃÕ")         <> 0 THEN IntS7072.RegisteredProvCd = "Êº".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊÔ§ËìºØÃÕ")       <> 0 THEN IntS7072.RegisteredProvCd = "ÊË".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊØâ¢·ÑÂ")         <> 0 THEN IntS7072.RegisteredProvCd = "Ê·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊØ¾ÃÃ³ºØÃÕ")      <> 0 THEN IntS7072.RegisteredProvCd = "Ê¾".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊØÃÒÉ®Ãì¸Ò¹Õ")    <> 0 THEN IntS7072.RegisteredProvCd = "Ê®".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÊØÃÔ¹·Ãì")        <> 0 THEN IntS7072.RegisteredProvCd = "ÊÃ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"Ë¹Í§¤ÒÂ")         <> 0 THEN IntS7072.RegisteredProvCd = "¹¤".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"Ë¹Í§ºÑÇÅÓÀÙ")     <> 0 THEN IntS7072.RegisteredProvCd = "¹À".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍèÒ§·Í§")         <> 0 THEN IntS7072.RegisteredProvCd = "Í·".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍÓ¹Ò¨à¨ÃÔ­")      <> 0 THEN IntS7072.RegisteredProvCd = "Í¨".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍØ´Ã¸Ò¹Õ")        <> 0 THEN IntS7072.RegisteredProvCd = "Í´".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍØµÃ´Ôµ¶ì")       <> 0 THEN IntS7072.RegisteredProvCd = "Íµ".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍØ·ÑÂ¸Ò¹Õ")       <> 0 THEN IntS7072.RegisteredProvCd = "Í¹".
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ÍØºÅÃÒª¸Ò¹Õ")     <> 0 THEN IntS7072.RegisteredProvCd = "Íº". 
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"ºÖ§¡ÒÌ")          <> 0 THEN IntS7072.RegisteredProvCd = "º¡". 
/*[EXECUTABLE]*/                     ELSE IF index(EntS7072.RegisteredProvCd,"àºµ§")            <> 0 THEN IntS7072.RegisteredProvCd = "ºµ". 
/*[BLANK]*/                          
/*[EXECUTABLE]*/                     OUTPUT TO Chk_RegProvCd_Error.txt   APPEND. 
/*[EXECUTABLE]*/                     PUT "PD_ChkDataProvincd : " TODAY FORMAT "99/99/9999" STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                         "Company:"                   EntS7072.company          FORMAT "X(10)"  
/*[EXECUTABLE]*/                         "ContractNumber:"            EntS7072.ContractNumber   FORMAT "X(30)"  
/*[EXECUTABLE]*/                         "EntS7072.RegisteredProvCd:" EntS7072.RegisteredProvCd FORMAT "X(35)"  
/*[EXECUTABLE]*/                         "IntS7072.RegisteredProvCd:" IntS7072.RegisteredProvCd FORMAT "X(35)" SKIP. 
/*[EXECUTABLE]*/                     OUTPUT CLOSE. 
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ClearData C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ClearData :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                     ASSIGN    fi_PolicyNumber = "" fi_Vehicle     = "" fi_vehreg = ""
/*[EXECUTABLE]*/                     fi_RegisteredProvinceCode = "" fi_PlateNumber = ""
/*[EXECUTABLE]*/                     .
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ConDBExp C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ConDBExp :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[COMMENT]*/                       /*      19.30.00 ¹.      07.30.00 ¹.*/
/*[EXECUTABLE]*/                 IF TIME >= 70200 OR TIME <= 27000 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF CONNECTED ("expiry") THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     DISCONNECT expiry NO-ERROR.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     OUTPUT TO DBExpiry.TXT APPEND.
/*[EXECUTABLE]*/                     PUT "PD_SAVEPD1: 1. DISCONNECT DB Expiry: " FORMAT "x(36)"
/*[EXECUTABLE]*/                          CONNECTED ("expiry")
/*[EXECUTABLE]*/                         " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF NOT CONNECTED ("expiry") THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     RUN WRS/WRSGU1DB.P
/*[EXECUTABLE]*/                          (""  /*Userid*/
/*[EXECUTABLE]*/                          ,""  /*Password*/
/*[EXECUTABLE]*/                          ,"expiry"). /*Database name*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     OUTPUT TO DBExpiry.TXT APPEND.
/*[EXECUTABLE]*/                     PUT "PD_SAVEPD1: 2. CONNECTED DB Expiry: " FORMAT "x(36)"
/*[EXECUTABLE]*/                          CONNECTED ("expiry")
/*[EXECUTABLE]*/                         " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pd_DeleteResultX C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pd_DeleteResultX :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_rec_rq AS RECID     NO-UNDO.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[EXECUTABLE]*/                 ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FOR EACH IntS7072Result WHERE
/*[EXECUTABLE]*/                            IntS7072Result.SystemRq      = IntS7072.SystemRq
/*[EXECUTABLE]*/                        AND IntS7072Result.CompanyCode   = IntS7072.CompanyCode
/*[EXECUTABLE]*/                        AND IntS7072Result.PolicyNumber  = IntS7072.PolicyNumber
/*[EXECUTABLE]*/                   :
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     DELETE IntS7072Result.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   DELETE IntS7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   nv_rec_rq = 0.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RELEASE IntS7072Result.
/*[EXECUTABLE]*/                 RELEASE IntS7072.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_DispData C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_DispData :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                     DISPLAY fi_PolicyNumber            fi_Vehicle      fi_vehreg 
/*[EXECUTABLE]*/                             fi_RegisteredProvinceCode  fi_PlateNumber
/*[EXECUTABLE]*/                     WITH FRAME DEFAULT-FRAME.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_DispMess C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_DispMess :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_notfound AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 ASSIGN
/*[COMMENT]*/                        /*
/*[COMMENT]*/                        fi_notfound2 = "Please press button F4 = TO Exit. " 
/*[COMMENT]*/                                     + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").
/*[COMMENT]*/                        my-datetime  = STRING(DATETIME(TODAY, MTIME)).                  
/*[COMMENT]*/                        */
/*[EXECUTABLE]*/                     fi_notfound  = nv_notfound 
/*[EXECUTABLE]*/                     my-datetime  = SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12) + " "
/*[EXECUTABLE]*/                                  + SUBSTR(STRING(DATETIME(TODAY, MTIME)),1,10)
/*[EXECUTABLE]*/                     fi_notfound2 = my-datetime + "    Please press button F4 = TO Exit & ÃÍÊÑ¡¤ÃÙè ("
/*[EXECUTABLE]*/                                  + STRING(nv_CountLeave,">9") + ")".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     DISPLAY  fi_notfound fi_notfound2 FORMAT "X(78)" FGCOLOR 6 WITH FRAME DEFAULT-FRAME.
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_ErrorRunPol C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_ErrorRunPol :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_rec_rq     AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_msgerror   AS CHARACTER NO-UNDO.
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
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE IntS7072 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST EntS7072 WHERE
/*[EXECUTABLE]*/                              EntS7072.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                          AND EntS7072.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                          AND EntS7072.Username        = IntS7072.Username
/*[EXECUTABLE]*/                          AND EntS7072.Password        = IntS7072.Password
/*[EXECUTABLE]*/                          AND EntS7072.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[COMMENT]*/                           /*AND EntS7072.PolicyNumber = nv_PolicyV70 */
/*[EXECUTABLE]*/                   NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE EntS7072 THEN EntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IntS7072.ProcessStatus = "E".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN PD_SaveError (nv_msgerror).
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RUN PD_SaveError ("äÁè¾ºÃËÑÊ¾Ãº. " + IntS7072.CMIVehTypeCd + " ã¹¡ÅØèÁ§Ò¹·ÕèãËé¨Ñ´¨ÓË¹èÒÂ").
/*[COMMENT]*/                      */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RETURN.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_FNameAttach C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_FNameAttach :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_CompanyCode     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_PolicyType      AS CHARACTER NO-UNDO. /*v70,v72*/
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_CMIPolicyTypeCd AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd      AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_PrgName         AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg          AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VAR nv_ONLine    AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    nv_PolicyType  = "V70".
/*[COMMENT]*/                    nv_CompanyCode = "210".
/*[COMMENT]*/                    nv_CMIPolicyTypeCd = "3".
/*[COMMENT]*/                    */
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                   nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF IntPol7072.SERVICE_ID = "online" THEN DO:
/*[BLANK]*/                      
/*[COMMENT]*/                      /*ASN = ãºÃÑºÃÍ§¡ÒÃÃÑº»ÃÐ¡Ñ¹ÀÑÂ */
/*[EXECUTABLE]*/                   nv_ONLine = TRIM(nv_CompanyCode) + "cer".
/*[BLANK]*/                       
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST FNameAttach WHERE
/*[EXECUTABLE]*/                              FNameAttach.CompanyCode  = nv_ONLine
/*[EXECUTABLE]*/                          AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
/*[EXECUTABLE]*/                          AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                             /*AND FNameAttach.EffDate     <= TODAY*/
/*[EXECUTABLE]*/                          AND FNameAttach.SelectNumber = 1
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE FNameAttach THEN
/*[EXECUTABLE]*/                     ASSIGN 
/*[EXECUTABLE]*/                     nv_NameCompCd = FNameAttach.CompanyCode
/*[EXECUTABLE]*/                     nv_PrgName    = FNameAttach.PrgName
/*[EXECUTABLE]*/                     nv_PrmPrg     = FNameAttach.PrmPrg.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_NameCompCd = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     nv_ONLine = "ALLcer".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST FNameAttach WHERE
/*[EXECUTABLE]*/                                FNameAttach.CompanyCode  = nv_ONLine
/*[EXECUTABLE]*/                            AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
/*[EXECUTABLE]*/                            AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                               /*AND FNameAttach.EffDate     <= TODAY   */           /*08/01/2014*/
/*[EXECUTABLE]*/                            AND FNameAttach.SelectNumber = 1
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE FNameAttach THEN
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       nv_NameCompCd = "ALLcer"
/*[EXECUTABLE]*/                       nv_PrgName    = FNameAttach.PrgName
/*[EXECUTABLE]*/                       nv_PrmPrg     = FNameAttach.PrmPrg.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   FIND FIRST FNameAttach WHERE
/*[EXECUTABLE]*/                              FNameAttach.CompanyCode  = nv_CompanyCode
/*[EXECUTABLE]*/                          AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
/*[EXECUTABLE]*/                          AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                             /*AND FNameAttach.EffDate     <= TODAY*/
/*[EXECUTABLE]*/                          AND FNameAttach.SelectNumber = 1
/*[EXECUTABLE]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF AVAILABLE FNameAttach THEN
/*[EXECUTABLE]*/                     ASSIGN 
/*[EXECUTABLE]*/                     nv_NameCompCd = FNameAttach.CompanyCode
/*[EXECUTABLE]*/                     nv_PrgName    = FNameAttach.PrgName
/*[EXECUTABLE]*/                     nv_PrmPrg     = FNameAttach.PrmPrg.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_NameCompCd = "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST FNameAttach WHERE
/*[EXECUTABLE]*/                                FNameAttach.CompanyCode  = "ALL"
/*[EXECUTABLE]*/                            AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
/*[EXECUTABLE]*/                            AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[COMMENT]*/                               /*AND FNameAttach.EffDate     <= TODAY  */            /*08/01/2014*/
/*[EXECUTABLE]*/                            AND FNameAttach.SelectNumber = 1
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF AVAILABLE FNameAttach THEN
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       nv_NameCompCd = "ALL"
/*[EXECUTABLE]*/                       nv_PrgName    = FNameAttach.PrgName
/*[EXECUTABLE]*/                       nv_PrmPrg     = FNameAttach.PrmPrg.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 OUTPUT TO PD_FNameAttach.TXT APPEND.
/*[EXECUTABLE]*/                 PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     nv_CompanyCode     FORMAT "x(20)"
/*[EXECUTABLE]*/                     nv_PolicyType      FORMAT "x(20)"
/*[EXECUTABLE]*/                     nv_CMIPolicyTypeCd FORMAT "x(20)"
/*[EXECUTABLE]*/                   nv_NameCompCd FORMAT "x(20)" /*210*/
/*[EXECUTABLE]*/                   nv_PrgName    FORMAT "x(20)" /*Wctxr701A4*/
/*[EXECUTABLE]*/                   nv_PrmPrg     FORMAT "x(20)" /*V70A4*/
/*[EXECUTABLE]*/                   nv_ONLine     FORMAT "x(20)" /*V70A4*/
/*[EXECUTABLE]*/                 SKIP.
/*[EXECUTABLE]*/                 OUTPUT CLOSE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      DISPLAY nv_NameCompCd FORMAT "x(25)" /*210*/
/*[COMMENT]*/                              nv_PrgName    FORMAT "x(25)" /*Wctxr701A4*/
/*[COMMENT]*/                              nv_PrmPrg     FORMAT "x(25)" /*V70A4*/
/*[COMMENT]*/                      WITH 1 COLUMN.
/*[COMMENT]*/                    */
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_Futil C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_Futil :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /**/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER prm_PolicyTypeCd AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER prm_SumInsureAmt AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER prm_CompanyCode  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   DEFINE INPUT-OUTPUT PARAMETER nv_okprn      AS LOGICAL   NO-UNDO.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   DEFINE VARIABLE nv_UtilGrp    AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Sumins     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   /* ------------------------------------------------------------------------*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   nv_okprn = NO.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   /*§Ò¹»2.1 2.2 ·Ø¹ 1áÊ¹/ 2áÊ¹ ãËé¾ÔÁ¾ì ¡ÃÁ¸ÃÃÁìä´é*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   IF    prm_PolicyTypeCd = "2.1" OR prm_PolicyTypeCd = "2.6"
/*[UNCALLED]*/                      OR prm_PolicyTypeCd = "2.2" OR prm_PolicyTypeCd = "2.7"
/*[UNCALLED]*/                      OR prm_PolicyTypeCd = "2.3" OR prm_PolicyTypeCd = "2.8"
/*[UNCALLED]*/                      OR prm_PolicyTypeCd = "2.4" OR prm_PolicyTypeCd = "2.9"
/*[UNCALLED]*/                      OR prm_PolicyTypeCd = "2.5"
/*[UNCALLED]*/                   THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                          IF prm_PolicyTypeCd = "2.1" THEN nv_UtilGrp = "Prn21".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.2" THEN nv_UtilGrp = "Prn22".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.3" THEN nv_UtilGrp = "Prn23".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.4" THEN nv_UtilGrp = "Prn24".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.5" THEN nv_UtilGrp = "Prn25".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.6" THEN nv_UtilGrp = "Prn26".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.7" THEN nv_UtilGrp = "Prn27".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.8" THEN nv_UtilGrp = "Prn28".
/*[UNCALLED]*/                     ELSE IF prm_PolicyTypeCd = "2.9" THEN nv_UtilGrp = "Prn29".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     nv_Sumins = prm_SumInsureAmt.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     IF INDEX(nv_Sumins,".") <> 0 THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       nv_Sumins = SUBSTR(nv_Sumins,1,INDEX(nv_Sumins,".") - 1).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     nv_Sumins = REPLACE(nv_Sumins,",","") .
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     FIND FIRST FUtilSetUp WHERE
/*[UNCALLED]*/                                FUtilSetUp.UtilGrp     = nv_UtilGrp
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp1 = prm_PolicyTypeCd
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp2 = prm_CompanyCode
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp3 = "V70"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp4 = "Policy"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp5 = nv_Sumins
/*[UNCALLED]*/                            AND FUtilSetUp.EffDate    <= TODAY
/*[UNCALLED]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                     IF AVAILABLE FUtilSetUp THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       IF FUtilSetUp.UtilGrpCd1 = "YES" THEN nv_okprn = YES.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                   /**/
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_PUTError C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_PUTError :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_noerror AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 IF nv_noerror = "2" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                       TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "3" THEN DO:
/*[EXECUTABLE]*/                   OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                     TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "4" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "4. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                     TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "5" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "5. WRSGU10R.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                    TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                    " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "6" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                     TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "Rer" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO WRSGU100-ERROR-RENEW.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                     TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "PD01" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO PD_SAVEPD1_01.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "PD_SAVEPD1_01" FORMAT "x(13)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     nv_msgerror7072 FORMAT "x(130)"
/*[EXECUTABLE]*/                   SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                   RETURN. /*Add kridtiya i. ÂØº Loop */
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "PD1" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "PD_SAVEPD1: 1. START " FORMAT "x(25)"
/*[EXECUTABLE]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       " " IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                       " " IntPol7072.PreviousPolicyNumber
/*[EXECUTABLE]*/                   SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "PD2" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "PD_SAVEPD1: 2. END " FORMAT "x(25)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                   SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "EX1" THEN DO:
/*[EXECUTABLE]*/                   OUTPUT TO EXPIRY-DATA.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "1. EXPIRY " nv_PolicyV70 FORMAT "X(18)"
/*[EXECUTABLE]*/                       TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "EX2" THEN DO:
/*[EXECUTABLE]*/                   OUTPUT TO EXPIRY-DATA.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "2. EXPIRY: " nv_PolicyV70 FORMAT "X(18)"
/*[EXECUTABLE]*/                       TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_noerror = "EXNOTPRM1" THEN DO:
/*[EXECUTABLE]*/                   OUTPUT TO EXPIRY-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "1.EXPIRY ·Ø¹ /àºÕéÂ /covcod äÁèµÃ§: " nv_PolicyV70 FORMAT "X(16)"
/*[EXECUTABLE]*/                       "PreviousPolicyNumber: " IntPol7072.PreviousPolicyNumber FORMAT "X(16)"
/*[EXECUTABLE]*/                       TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pd_saveCer C-Win 
/*[EXECUTABLE]*/                 PROCEDURE Pd_saveCer :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_COPYTOFILE  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_RECIDcer    AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    CompanyCode    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    Username       AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    BrancdCd       AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    ContractNumber AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    PolicyNumber   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    nv_FileName    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE  VARIABLE    PolicyBinRs    AS MEMPTR    NO-UNDO. 
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
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SaveError C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SaveError :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_ErrorMessage  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND FIRST EntS7072Result WHERE
/*[EXECUTABLE]*/                                EntS7072Result.SystemRq        = IntS7072.SystemRq
/*[EXECUTABLE]*/                            AND EntS7072Result.CompanyCode     = IntS7072.CompanyCode
/*[EXECUTABLE]*/                            AND EntS7072Result.ContractNumber  = IntS7072.ContractNumber
/*[EXECUTABLE]*/                            AND EntS7072Result.keyRequestIndRq = IntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE EntS7072Result THEN DO:
/*[BLANK]*/                         
/*[EXECUTABLE]*/                       CREATE EntS7072Result.
/*[BLANK]*/                         
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       EntS7072Result.SystemRq             = IntS7072.SystemRq
/*[EXECUTABLE]*/                       EntS7072Result.InsurerId            = IntS7072.InsurerId
/*[EXECUTABLE]*/                       EntS7072Result.RqUID                = IntS7072.RqUID
/*[EXECUTABLE]*/                       EntS7072Result.keyRequestIndRq      = IntS7072.keyRequestIndRq
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.CompanyCode          = IntS7072.CompanyCode
/*[EXECUTABLE]*/                       EntS7072Result.ContractNumber       = IntS7072.ContractNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.ReferenceNumber      = IntS7072.ReferenceNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseRefNumber     = IntS7072.EndorseRefNumber
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.InsurerCode          = IntS7072.InsurerCode
/*[EXECUTABLE]*/                       EntS7072Result.MethodCode           = IntS7072.MethodCode
/*[EXECUTABLE]*/                       EntS7072Result.Policy               = IntS7072.Policy
/*[EXECUTABLE]*/                       EntS7072Result.Rencnt               = IntS7072.Rencnt
/*[EXECUTABLE]*/                       EntS7072Result.Endcnt               = IntS7072.Endcnt
/*[EXECUTABLE]*/                       EntS7072Result.Riskno               = IntS7072.Riskno
/*[EXECUTABLE]*/                       EntS7072Result.Itemno               = IntS7072.Itemno
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.ProcessByUser        = IntS7072.ProcessByUser
/*[EXECUTABLE]*/                       EntS7072Result.ProcessDate          = IntS7072.ProcessDate
/*[EXECUTABLE]*/                       EntS7072Result.ProcessTime          = IntS7072.ProcessTime
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntDate       = IntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                       EntS7072Result.TrnFromIntTime       = IntS7072.TrnFromIntTime
/*[EXECUTABLE]*/                       EntS7072Result.ReceiveNumber        = IntS7072.ReceiveNumber
/*[EXECUTABLE]*/                       EntS7072Result.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.RecordGUIDRs         = ""
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseDt   =   STRING( YEAR(IntS7072.TrnFromIntDate),"9999")
/*[EXECUTABLE]*/                                                                + STRING(MONTH(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                                                                + STRING(  DAY(IntS7072.TrnFromIntDate),"99")
/*[EXECUTABLE]*/                       EntS7072Result.TransactionResponseTime = IntS7072.TrnFromIntTime
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.PolicyNumber         = ""
/*[EXECUTABLE]*/                       EntS7072Result.DocumentUID          = ""
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.CMIPolicyNumber      = ""
/*[EXECUTABLE]*/                       EntS7072Result.CMIDocumentUID       = ""
/*[EXECUTABLE]*/                       EntS7072Result.MsgStatusCd          = "FAIL"
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       EntS7072Result.BranchCd             = IntS7072.BranchCd
/*[EXECUTABLE]*/                       EntS7072Result.vehreg               = IntS7072.vehreg
/*[EXECUTABLE]*/                       EntS7072Result.Adjustno             = 0
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.ResultStatus         = "FAIL"                 /*IntS7072.ResultStatus*/
/*[EXECUTABLE]*/                       EntS7072Result.ErrorCode            = ""
/*[EXECUTABLE]*/                       EntS7072Result.ErrorMessage         = nv_ErrorMessage
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       EntS7072Result.LinkStatus           = "E"      .
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER  nv_recinout7072   AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_trty11  AS CHARACTER NO-UNDO. /*M,T*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_docno1  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_STdocno AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_STKNo   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_okprn AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_OLDPolicy AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NewPolicy AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[EXECUTABLE]*/                 nv_msgerror7072  = "".   /* Add kridtiya i. */
/*[EXECUTABLE]*/                 RUN PD_ConDBExp.         /* DB EXPIRY */
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[EXECUTABLE]*/                 nv_resulttext = "".
/*[EXECUTABLE]*/                 nv_octets = "".
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[COMMENT]*/                    /*¡ÃÁ¸ÃÃÁì»... + ¾Ãº. */
/*[EXECUTABLE]*/                 IF (IntS7072.PolicyTypeCd    <> "" ) AND (IntS7072.RateGroup    <> "" ) AND  /*Add kridtiya i. */
/*[EXECUTABLE]*/                    (IntS7072.CMIPolicyTypeCd <> "" ) AND (IntS7072.CMIVehTypeCd <> "" )
/*[EXECUTABLE]*/                 THEN DO:
/*[COMMENT]*/                      /*RUN PD_SAVEPD17072.*/
/*[EXECUTABLE]*/                   FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70  /*70,72*/
/*[EXECUTABLE]*/                   NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF NOT AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                     RUN WRS/WRSDigit.p (output nv_octets).
/*[EXECUTABLE]*/                     CREATE IntPol7072.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE nv_octets = IntPol7072.RqUID.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN DO:
/*[EXECUTABLE]*/                   nv_octets = "".
/*[EXECUTABLE]*/                   FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70 /*70,72*/
/*[EXECUTABLE]*/                   NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   IF NOT AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                     RUN WRS/WRSDigit.p (output nv_octets).
/*[EXECUTABLE]*/                     CREATE IntPol7072.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE nv_octets = IntPol7072.RqUID.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:   /* ¡ÃÁ¸ÃÃÁì ¾Ãº.*/
/*[EXECUTABLE]*/                   IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                     FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72
/*[EXECUTABLE]*/                     NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                     IF NOT AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                       nv_octets = "".
/*[EXECUTABLE]*/                       RUN WRS/WRSDigit.p (output nv_octets).
/*[EXECUTABLE]*/                       CREATE IntPol7072.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE nv_octets = IntPol7072.RqUID.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE nv_msgerror7072 = "¾º¢éÍÁÙÅ ¾Ãº.äÁè¶Ù¡µéÍ§ CMIPolicyTypeCd /CMIVehTypeCd à»ç¹¤èÒÇèÒ§".
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 nv_RecIntPol7072 = RECID(IntPol7072).
/*[EXECUTABLE]*/                 nv_recinout7072  = RECID(IntPol7072).
/*[EXECUTABLE]*/                 RUN PD_SAVEPD2.    /*Save data to IntPol7072*/
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[COMMENT]*/                    /* ËÒàÅ¢ Docno1 */
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    /*20/3/2015*/
/*[EXECUTABLE]*/                 nv_okprn = NO.
/*[EXECUTABLE]*/                 RUN WRS\WRSFutil.p 
/*[EXECUTABLE]*/                     (IntPol7072.PolicyTypeCd
/*[EXECUTABLE]*/                     ,IntPol7072.SumInsureAmt
/*[EXECUTABLE]*/                     ,IntPol7072.CompanyCode
/*[EXECUTABLE]*/                     ,INPUT-OUTPUT nv_okprn).
/*[COMMENT]*/                    /* ---------------------------------------------------- */
/*[EXECUTABLE]*/                 RUN PD_SAVEPD1_01 (nv_okprn 
/*[EXECUTABLE]*/                                   ,nv_trty11            
/*[EXECUTABLE]*/                                   ,nv_docno1            
/*[EXECUTABLE]*/                                   ,nv_STdocno
/*[EXECUTABLE]*/                                   ,nv_STKNo).  /*Add kridtiya i. ÂØº Loop */
/*[EXECUTABLE]*/                 IF IntPol7072.CMIDocumentUID <> "" THEN IntS7072.CMIDocumentUID = IntPol7072.CMIDocumentUID .
/*[EXECUTABLE]*/                 IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                   RUN PD_PUTError ("PD01").
/*[EXECUTABLE]*/                   RETURN. /*Add kridtiya i. ÂØº Loop */
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 IF IntPol7072.GenSicBranST   = "ERROR" THEN RETURN. /*Add kridtiya i. ÂØº Loop */
/*[COMMENT]*/                    /* ----------------------------------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN PD_PUTError ("PD1").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 nv_Renew = NO.  /*A57-0300: §Ò¹µèÍÍÒÂØ*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF  (SUBSTR(IntPol7072.PolicyNumber,1,1)    =  "R") OR 
/*[EXECUTABLE]*/                     (IntPol7072.PolicyStatus  = "R" AND IntPol7072.PreviousPolicyNumber <> "")
/*[EXECUTABLE]*/                     AND    IntPol7072.PreviousPolicyNumber <> ""
/*[EXECUTABLE]*/                 THEN nv_Renew = YES.
/*[EXECUTABLE]*/                 IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN nv_Renew = NO. /*¾Ãº.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
/*[EXECUTABLE]*/                 nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
/*[COMMENT]*/                    /*nv_msgerror7072  = "".*/ /* Add kridtiya i. Â¡ä» ´éÒ¹º¹ÊØ´ */
/*[COMMENT]*/                    /* Generate policy, uwm100, uwm120, uwm301, uwm130, uwd132 */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN DO: 
/*[EXECUTABLE]*/                    RUN PD_SAVEPD1_02.   /*Add kridtiya i. 70 + ¾Ãº. */
/*[EXECUTABLE]*/                    IF nv_msgerror7072 <> "" THEN RETURN.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE IF IntPol7072.PolicyTypeCd <> "1" 
/*[EXECUTABLE]*/                     AND IntPol7072.PolicyTypeCd <> "2"
/*[EXECUTABLE]*/                     AND IntPol7072.PolicyTypeCd <> "3"
/*[EXECUTABLE]*/                     AND nv_Renew = NO  /*Add by Kridtiya i. 20/06/2017 */
/*[EXECUTABLE]*/                 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF    IntPol7072.PolicyTypeCd = ""
/*[EXECUTABLE]*/                      OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[EXECUTABLE]*/                      OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[COMMENT]*/                         /**/
/*[EXECUTABLE]*/                      OR IntPol7072.CMIPolicyTypeCd = "110"
/*[EXECUTABLE]*/                      OR IntPol7072.CMIPolicyTypeCd = "120A"
/*[EXECUTABLE]*/                      OR IntPol7072.CMIPolicyTypeCd = "140A"
/*[EXECUTABLE]*/                   THEN DO:
/*[EXECUTABLE]*/                     IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                       nv_msgerror7072 = "".
/*[EXECUTABLE]*/                       RUN UZ/UZO7201WS.P
/*[EXECUTABLE]*/                            (nv_RecIntPol7072
/*[EXECUTABLE]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                       NO-ERROR.
/*[EXECUTABLE]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                         RUN PD_PUTError ("2").
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END. /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
/*[EXECUTABLE]*/                     IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[EXECUTABLE]*/                        OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[EXECUTABLE]*/                     THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN WRS/WRSGU100.P
/*[EXECUTABLE]*/                            (nv_RecIntPol7072
/*[EXECUTABLE]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN PD_PUTError ("3").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[EXECUTABLE]*/                     IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN UZ/UZO7201WS.P
/*[EXECUTABLE]*/                            (nv_RecIntPol7072
/*[EXECUTABLE]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                       NO-ERROR.
/*[EXECUTABLE]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN PD_PUTError ("4").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[COMMENT]*/                      /* A57-0300: §Ò¹µèÍÍÒÂØ */
/*[COMMENT]*/                      /* ISUZU */
/*[EXECUTABLE]*/                   IF     IntPol7072.PolicyStatus  = "R"   AND IntPol7072.PreviousPolicyNumber <> ""
/*[EXECUTABLE]*/                      AND IntPol7072.PolicyTypeCd <> ""  
/*[COMMENT]*/                       /*  AND IntPol7072.CompanyCode   = "476" AND SUBSTR(IntPol7072.PolicyNumber,1,2) = "74"*/ /*comment by Kridtiya i. 17/06/2017*/
/*[EXECUTABLE]*/                   THEN nv_Renew = YES.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   ASSIGN 
/*[EXECUTABLE]*/                   nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
/*[EXECUTABLE]*/                   nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_Renew = YES THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF NOT CONNECTED ("expiry") THEN DO:
/*[EXECUTABLE]*/                       RUN WRS/WRSGU1DB.P
/*[EXECUTABLE]*/                            (""  /*Userid*/
/*[EXECUTABLE]*/                           ,""  /*Password*/
/*[EXECUTABLE]*/                           ,"expiry"). /*Database name*/
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF CONNECTED ("expiry") THEN DO:
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
/*[EXECUTABLE]*/                       RUN WRS/WRSGU10R.P
/*[EXECUTABLE]*/                              (nv_RecIntPol7072
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                       NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN PD_PUTError ("Rer").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /********** comment by Kridtiya i. 26/08/2017   /*ISUZU*/
/*[COMMENT]*/                          IF IntPol7072.CompanyCode = "476" OR IntPol7072.CompanyCode = "833"  OR IntPol7072.CompanyCode = "834" THEN DO: /*Add by Kridtiya i. 17/06/2017*/
/*[COMMENT]*/                              */
/*[BLANK]*/                            
/*[EXECUTABLE]*/                         IF nv_msgerror7072 = "" THEN DO:
/*[EXECUTABLE]*/                           ASSIGN
/*[EXECUTABLE]*/                           nv_expiryrencnt = 0  nv_expirysigr_p = 0  nv_expiryprem_t = 0
/*[EXECUTABLE]*/                           nv_expirysclass = "" nv_expirycovcod = "" nv_PrnRenew = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           RUN WRS/WRSGUCKR.P /*Check ·Ø¹ /àºÕéÂ /covcod ·Õè sic_bran µÃ§ËÃ×ÍäÁè*/
/*[EXECUTABLE]*/                             (nv_RecIntPol7072
/*[COMMENT]*/                                 /**/
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_expiryrencnt
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_expirysigr_p
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_expiryprem_t
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_expirysclass
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_expirycovcod
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_PrnRenew
/*[COMMENT]*/                                 /**/
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           RUN PD_PUTError ("EX2").
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           nv_msgerror7072 = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF IntPol7072.PolicyTypeCd = "1" THEN nv_PrnRenew = "YES".
/*[BLANK]*/                      
/*[COMMENT]*/                              /* Change R -> D */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF nv_PrnRenew = "YES" THEN DO: /*·Ø¹ /àºÕéÂ /covcod µÃ§*/
/*[COMMENT]*/                                  /* àÅ¢ R */
/*[EXECUTABLE]*/                             IF (SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R") AND (IntPol7072.CompanyCode = "476") THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               nv_NewPolicy = "".
/*[EXECUTABLE]*/                               nv_OLDPolicy = IntPol7072.PolicyNumber.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               RUN WSP/WSPRunP2WS.P  (INPUT  IntPol7072.CompanyCode
/*[EXECUTABLE]*/                                                     ,INPUT  IntPol7072.BranchCd
/*[EXECUTABLE]*/                                                     ,INPUT  "V70"
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_NewPolicy
/*[EXECUTABLE]*/                                                     ,OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                               NO-ERROR NO-WAIT.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               OUTPUT TO WRSBQ7072-Renew.TXT APPEND.
/*[EXECUTABLE]*/                               PUT "Change R -> D: " IntPol7072.CompanyCode IntPol7072.BranchCd
/*[EXECUTABLE]*/                                   nv_OLDPolicy FORMAT "x(16)" nv_NewPolicy FORMAT "x(16)"
/*[EXECUTABLE]*/                                   SKIP.
/*[EXECUTABLE]*/                               OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               IF nv_msgerror7072 = "" AND nv_NewPolicy <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                                 RUN WRS/WRSCngPoL.P
/*[EXECUTABLE]*/                                   (INPUT nv_OLDPolicy
/*[EXECUTABLE]*/                                   ,INPUT nv_expiryrencnt
/*[EXECUTABLE]*/                                   ,INPUT 0 /*endcnt*/
/*[EXECUTABLE]*/                                   ,INPUT nv_NewPolicy
/*[COMMENT]*/                                       /**/
/*[EXECUTABLE]*/                                   ,OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                               END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               IF nv_msgerror7072 = "" AND nv_NewPolicy <> ""  THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                                 IF AVAILABLE IntPol7072 THEN
/*[EXECUTABLE]*/                                   ASSIGN
/*[EXECUTABLE]*/                                   nv_PolicyV70 = nv_NewPolicy
/*[EXECUTABLE]*/                                   IntPol7072.PolicyNumber = nv_NewPolicy
/*[EXECUTABLE]*/                                   IntPol7072.ConfirmBy = "AUTO".
/*[BLANK]*/                      
/*[COMMENT]*/                                    /*??? àÅ¢ docno ËÒÍÂèÒ§äÃ*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                               END.
/*[EXECUTABLE]*/                             END. /*IF SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R"*/ 
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           END. /* nv_PrnRenew = "YES" */
/*[EXECUTABLE]*/                           ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             nv_msgerror7072 = "Not renew: " + nv_PrnRenew.
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[COMMENT]*/                          /*END. /*IF IntPol7072.CompanyCode = "476" THEN DO:*//* comment by Kridtiya i. 15/07/2017*/*/ /*comment by Kridtiya i. 26/08/2017 */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE  nv_msgerror7072 = "Not Connect Expiry: ".
/*[COMMENT]*/                        /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_PrnRenew = "" THEN DO: /*·Ø¹ /àºÕéÂ /covcod äÁèµÃ§*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                       NO-ERROR.
/*[EXECUTABLE]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN PD_PUTError ("5").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         nv_msgerror7072 = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN PD_PUTError ("EXNOTPRM1"). /*·Ø¹ /àºÕéÂ /covcod äÁèµÃ§*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN WRS/WRSGU100.P
/*[EXECUTABLE]*/                            (nv_RecIntPol7072
/*[EXECUTABLE]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                     NO-ERROR.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[COMMENT]*/                        /*äÁèãªè§Ò¹ Renew*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF IntPol7072.PolicyTypeCd <> "1"  THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       RUN WRS/WRSGU100.P
/*[EXECUTABLE]*/                          (nv_RecIntPol7072
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.PolicyTypeCd = "1" THEN DO:
/*[EXECUTABLE]*/                         nv_acno1 = "".
/*[COMMENT]*/                            /* ËÒ ÃËÑÊµÑÇá·¹  */
/*[EXECUTABLE]*/                         RUN WSP/WSPMCpny.P 
/*[EXECUTABLE]*/                             (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                             ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                             ,"V" + SUBSTR(IntPol7072.PolicyNumber,3,2) /*nv_Poltyp*/
/*[EXECUTABLE]*/                             ,OUTPUT crCompanyNo
/*[EXECUTABLE]*/                             ,OUTPUT crBranchNo
/*[EXECUTABLE]*/                             ,OUTPUT nv_acno1
/*[EXECUTABLE]*/                             ,OUTPUT nv_agent
/*[EXECUTABLE]*/                             ,OUTPUT nv_msgerror7072
/*[EXECUTABLE]*/                             ).
/*[COMMENT]*/                            /**/
/*[EXECUTABLE]*/                         IF nv_acno1 <> "" THEN DO:
/*[EXECUTABLE]*/                           FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                           NO-ERROR.
/*[EXECUTABLE]*/                           IF AVAILABLE IntPol7072 THEN 
/*[EXECUTABLE]*/                             IntPol7072.AgentBrokerLicenseNumber = nv_acno1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN WRS/WRSGU100.P
/*[EXECUTABLE]*/                             (nv_RecIntPol7072
/*[EXECUTABLE]*/                             ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_msgerror7072 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     RUN PD_PUTError ("6").
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                     NO-ERROR.
/*[EXECUTABLE]*/                     IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                       IntPol7072.GenSicBran     = NO
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranText = nv_msgerror7072
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranST   = "ERROR"
/*[EXECUTABLE]*/                       IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[EXECUTABLE]*/                       IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                          IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     RETURN.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN PD_PUTError ("PD2").
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                   IntPol7072.GenSicBran   = YES
/*[EXECUTABLE]*/                   IntPol7072.GenSicBranBy = "WRSBQ7072.W"
/*[EXECUTABLE]*/                   IntPol7072.GenSicBranDt = TODAY
/*[EXECUTABLE]*/                   IntPol7072.GenSicBranTime = STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                   IntPol7072.GenSicBranText = ""
/*[EXECUTABLE]*/                   IntPol7072.GenSicBranST = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN IntPol7072.TransferToPremium = YES. /*kridtiya i.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                     IntPol7072.GenSicBran   = NO
/*[EXECUTABLE]*/                     IntPol7072.GenSicBranText = nv_msgerror7072
/*[EXECUTABLE]*/                     IntPol7072.GenSicBranST = "ERROR"
/*[EXECUTABLE]*/                     IntPol7072.ErrorMessage = nv_msgerror7072.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                        IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   OUTPUT STREAM xmlstream TO PUT_WRSBQCuw3.TXT.
/*[EXECUTABLE]*/                   PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                       " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                   OUTPUT STREAM xmlstream CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN WRS\WRSBQCuw3.P (nv_RecIntPol7072). /*à¡çº·Ø¹ àºÕéÂ ÍÒ¡Ã áÊµÁ»ì µÍº¡ÅÑº*/
/*[EXECUTABLE]*/                   OUTPUT STREAM xmlstream TO PUT_SAVEPD1IntS.TXT.
/*[EXECUTABLE]*/                   PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                       " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                   OUTPUT STREAM xmlstream CLOSE.
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1IntS (nv_rec_rq, nv_RecIntPol7072). /*à¡çº·Ø¹ àºÕéÂ ÍÒ¡Ã áÊµÁ»ì µÍº¡ÅÑº*/
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                 NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAILABLE IntPol7072 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF IntPol7072.ConfirmBy <> "AUTO" THEN DO:
/*[COMMENT]*/                        /*»2.1 2.2 ·Ø¹µèÓ¡ÇèÒ1áÊ¹ÍÍ¡¡ÃÁ¸ÃÃÁì D ËÃ×ÍàÅ¢ÊÒ¢Ò 2 ËÅÑ¡*/
/*[EXECUTABLE]*/                     IF (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[EXECUTABLE]*/                        AND  nv_okprn = YES
/*[EXECUTABLE]*/                     THEN DO:
/*[COMMENT]*/                          /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                         /* IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN /*Add kridtiya i. 70+72*/
/*[COMMENT]*/                            RUN PD_SAVEPD1FileAttach2 (nv_RecIntPol7072
/*[COMMENT]*/                                                   ,nv_rec_rq ).
/*[EXECUTABLE]*/                       ELSE *//*Kridtiya i. */
/*[EXECUTABLE]*/                         IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN DO:
/*[EXECUTABLE]*/                             IF   IntPol7072.SERVICE_ID = "online" AND IntPol7072.CompanyCode = "834" THEN  
/*[EXECUTABLE]*/                                 RUN PD_SAVEPD1FileAtt834 (nv_RecIntPol7072
/*[EXECUTABLE]*/                                                           ,nv_rec_rq ).
/*[EXECUTABLE]*/                             ELSE 
/*[EXECUTABLE]*/                                 RUN PD_SAVEPD1FileAttach (nv_RecIntPol7072
/*[EXECUTABLE]*/                                                           ,nv_rec_rq ).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                         IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN  
/*[EXECUTABLE]*/                             RUN PD_SAVEPD1ChkVehicle. /*Êè§µÃÇ¨ÊÀÒ¾Ã¶*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     END. 
/*[EXECUTABLE]*/                   END. /*IF IntS7072.ConfirmBy <> "AUTO" */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF IntPol7072.ConfirmBy = "AUTO" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" AND IntPol7072.SERVICE_ID <> "PAYMENT" THEN 
/*[EXECUTABLE]*/                        RUN WRS\WRSGwCtx 
/*[EXECUTABLE]*/                                (IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                                ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                                ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                                ,0  /*RECID(uwm100)*/
/*[EXECUTABLE]*/                                ,IntPol7072.CompanyCode  /*833*/
/*[EXECUTABLE]*/                                ,IntPol7072.PolicyTypeCd /*2.2*/
/*[EXECUTABLE]*/                                ,IntPol7072.RateGroup).  /*110*/ /*kridtiya i.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     ELSE DO: /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          OUTPUT TO EXPIRT-ERROR.TXT APPEND.
/*[COMMENT]*/                          PUT "7. EXPIRT " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[COMMENT]*/                              TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                              " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[COMMENT]*/                          OUTPUT CLOSE.
/*[COMMENT]*/                          */
/*[EXECUTABLE]*/                       IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.CompanyCode = "834" AND IntPol7072.SERVICE_ID <> "PAYMENT" THEN 
/*[EXECUTABLE]*/                           RUN WRS\WRSGwCtx 
/*[EXECUTABLE]*/                                (IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                                ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                                ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                                ,0  /*RECID(uwm100)*/
/*[EXECUTABLE]*/                                ,IntPol7072.CompanyCode  /*833*/
/*[EXECUTABLE]*/                                ,IntPol7072.PolicyTypeCd /*2.2*/
/*[EXECUTABLE]*/                                ,IntPol7072.RateGroup).
/*[EXECUTABLE]*/                       ELSE IF   IntPol7072.SERVICE_ID = "online" AND IntPol7072.CompanyCode = "834" THEN  
/*[EXECUTABLE]*/                           RUN PD_SAVEPD1FileAtt834 (nv_RecIntPol7072
/*[EXECUTABLE]*/                                                     ,nv_rec_rq ).
/*[EXECUTABLE]*/                       ELSE DO:
/*[EXECUTABLE]*/                           IF IntPol7072.SERVICE_ID <> "PAYMENT" THEN DO:
/*[EXECUTABLE]*/                               RUN PD_SAVEPD1FileAttach
/*[EXECUTABLE]*/                                   (nv_RecIntPol7072
/*[EXECUTABLE]*/                                    ,nv_rec_rq ).  /*RECID(IntS7072)*/
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.  /*IF AVAILABLE IntPol7072 */
/*[EXECUTABLE]*/                 OUTPUT TO PD_SavePD1_CK.TXT APPEND.
/*[EXECUTABLE]*/                   PUT "nv_Renew :" nv_Renew IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                       TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                       "nv_msgerror7072 : " nv_msgerror7072  FORMAT "X(150)" SKIP
/*[EXECUTABLE]*/                       "service " IntPol7072.SERVICE_ID  FORMAT "X(15)" SKIP
/*[EXECUTABLE]*/                       "Auto :"   IntPol7072.ConfirmBy   FORMAT "X(15)" SKIP
/*[EXECUTABLE]*/                       " PrnRenew: "  nv_PrnRenew  " msgerror7072: " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                   OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RELEASE IntS7072.
/*[EXECUTABLE]*/                 RELEASE IntPol7072.
/*[EXECUTABLE]*/                 RELEASE IntQPolicy.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD17072 C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD17072 :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /* 20/03/2016
/*[UNCALLED]*/                   nv_octets = "".
/*[UNCALLED]*/                   FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70  /*70,72*/
/*[UNCALLED]*/                       NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       RUN WRS/WRSDigit.p (output nv_octets).
/*[UNCALLED]*/                       CREATE IntPol7072.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   ELSE nv_octets = IntPol7072.RqUID.
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72  /*72*/
/*[UNCALLED]*/                       NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       nv_octets = "".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       RUN WRS/WRSDigit.p (output nv_octets).
/*[UNCALLED]*/                       CREATE IntPol7072.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   ELSE nv_octets = IntPol7072.RqUID.
/*[UNCALLED]*/                        */
/*[UNCALLED]*/                   ---- */
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1ChkFile C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1ChkFile :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:           RUN PD_CheckFpdf (INPUT nv_NameCompCd
/*[COMMENT]*/                                         ,INPUT "V72"
/*[COMMENT]*/                                         ,INPUT IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                                         ,OUTPUT nv_SAVEmsgerror).
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 DEFINE INPUT  PARAMETER nv_NameCompCd        AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT  PARAMETER nv_PolicyType        AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT  PARAMETER nv_PolicyTypeCd      AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE OUTPUT PARAMETER nv_SAVEmsgerror      AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LineSeqno     AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lwaitcount    AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LcountAgain   AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lcount        AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_StartCount    AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LastCount     AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SearchDrive      AS CHARACTER NO-UNDO. /*"C:\"*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SearchName       AS CHARACTER NO-UNDO. /*"AcroRd32.exe"*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CreateFileAcroRd AS LOGICAL   NO-UNDO. /*YES/NO*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CheckFile        AS CHARACTER NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_savefile         AS CHARACTER FORMAT "X(50)"  NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_savesize         AS DECIMAL   INITIAL 0       NO-UNDO. /*¢¹Ò´ file ·Õèä´é*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_firstsize        AS DECIMAL   FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0 NO-UNDO. /*¢¹Ò´ file ·Õè¾º¤ÃÑé§áÃ¡*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CheckCount       AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CheckConfirm     AS LOGICAL   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_WaitCount        AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_WaitTotal        AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_CountChkFile     AS INTEGER   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_ERRORCount       AS INTEGER   NO-UNDO.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 nv_INPUTFileName = "".
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    nv_COPYTOFILE    = "".*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FOR EACH FNameAttach WHERE
/*[EXECUTABLE]*/                          FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                      AND FNameAttach.PolicyTypeCd = nv_PolicyType   /*"V72"*/
/*[EXECUTABLE]*/                      AND FNameAttach.CoverTypeCd  = nv_PolicyTypeCd /*IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/*/
/*[EXECUTABLE]*/                      AND FNameAttach.EffDate     <= TODAY
/*[EXECUTABLE]*/                 NO-LOCK
/*[EXECUTABLE]*/                 BREAK BY FNameAttach.SelectNumber
/*[EXECUTABLE]*/                 :
/*[EXECUTABLE]*/                   IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                    ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   LEAVE.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 IF nv_INPUTFileName = "" /* OR nv_COPYTOFILE = ""*/ THEN RETURN.
/*[COMMENT]*/                    /* --------------------------------------------------------- */ 
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 nv_SearchDrive       = "D:"
/*[COMMENT]*/                    /*
/*[EXECUTABLE]*/                 nv_SearchName        = "TESTCHECKFILE.txt"  /*"sombatp.PDF" */ */
/*[EXECUTABLE]*/                 nv_SearchName        = nv_INPUTFileName
/*[EXECUTABLE]*/                 nv_CreateFileAcroRd  = yes
/*[EXECUTABLE]*/                 nv_CheckFile         = ""
/*[EXECUTABLE]*/                 nv_firstsize         = 0
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 nv_CheckCount   = 0
/*[EXECUTABLE]*/                 nv_CheckConfirm = NO
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 nv_CountChkFile = 0
/*[EXECUTABLE]*/                 nv_WaitCount    = 0
/*[EXECUTABLE]*/                 nv_WaitTotal    = 1000000.  /* 1/2 ÇÔ¹Ò·Õ*/ 
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
/*[EXECUTABLE]*/                 loop_ChkFile:
/*[EXECUTABLE]*/                 REPEAT:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   nv_WaitCount    = 0.
/*[EXECUTABLE]*/                   DO WHILE nv_WaitCount <= nv_WaitTotal: /* 1/2 ÇÔ¹Ò·Õ*/
/*[EXECUTABLE]*/                     nv_WaitCount = nv_WaitCount + 1.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   nv_savefile = "".
/*[EXECUTABLE]*/                   nv_savesize = 0.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN WRS\WRSChkFile.P 
/*[EXECUTABLE]*/                     (
/*[EXECUTABLE]*/                      INPUT        nv_SearchDrive
/*[EXECUTABLE]*/                     ,INPUT        nv_SearchName  
/*[EXECUTABLE]*/                     ,INPUT        YES /*nv_CreateFileAcroRd*/
/*[EXECUTABLE]*/                     ,INPUT-OUTPUT nv_CheckFile
/*[EXECUTABLE]*/                     ,INPUT-OUTPUT nv_savefile
/*[EXECUTABLE]*/                     ,INPUT-OUTPUT nv_savesize
/*[EXECUTABLE]*/                     ).
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
/*[EXECUTABLE]*/                   nv_CountChkFile = nv_CountChkFile + 1.
/*[COMMENT]*/                    /*
/*[COMMENT]*/                      OUTPUT TO WRSChkFile.txt APPEND.
/*[COMMENT]*/                      PUT nv_CountChkFile nv_CheckCount " Filename: " nv_SearchName FORMAT "X(16)" " size file: " nv_savesize
/*[COMMENT]*/                          " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[COMMENT]*/                      SKIP.
/*[COMMENT]*/                      OUTPUT CLOSE.*/
/*[COMMENT]*/                      /* ------------------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_savefile = "" THEN DO: 
/*[EXECUTABLE]*/                     nv_ERRORCount = nv_ERRORCount + 1.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_ERRORCount >= 20 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       nv_savefile = "ERROR".
/*[EXECUTABLE]*/                       LEAVE loop_ChkFile.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     NEXT loop_ChkFile.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF nv_savefile = nv_SearchName THEN DO: /*ª×èÍfile·Õè¤é¹ËÒä´é à·ÕèÂº¡Ñº ª×èÍ file ·ÕèËÒ*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_savesize = 0 THEN NEXT loop_ChkFile.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_savesize <> 0 THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       nv_CheckCount = nv_CheckCount + 1. /*¹Ñº¡ÒÃä´é¢¹Ò´ file ÁÒ¡¡ÇèÒ 0*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_firstsize <> nv_savesize THEN nv_firstsize = nv_savesize.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*       nv_WaitTotal    = 850000. /* äÁè¶Ö§ 1/2 ÇÔ¹Ò·Õ*/ */
/*[EXECUTABLE]*/                        nv_WaitTotal    = 300000. /* äÁè¶Ö§ 1/2 ÇÔ¹Ò·Õ*/
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_CheckCount >= 2 THEN DO: /*¤é¹¤ÃÑé§·Õè 2 à¾×èÍ¤ÇÒÁá¹èã¨ÇèÒ ¢¹Ò´ file à·èÒà´ÔÁ*/
/*[COMMENT]*/                        /*IF nv_CheckCount >= 1 THEN DO:   /* Test by kridtiya i. 30/05/2017*/*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       nv_CheckConfirm = YES.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF nv_firstsize = nv_savesize THEN /*¢¹Ò´ file à·èÒà´ÔÁ*/
/*[EXECUTABLE]*/                         LEAVE loop_ChkFile.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     NEXT loop_ChkFile.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   LEAVE loop_ChkFile.
/*[EXECUTABLE]*/                 END. /*loop_ChkFile*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 PAUSE 2 NO-MESSAGE.   /*Add by Kridtiya i. wait certificate*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 OUTPUT TO WRSChkFile.txt APPEND.
/*[EXECUTABLE]*/                 PUT nv_CountChkFile nv_CheckCount " Filename: " nv_SearchName FORMAT "X(16)" " size file: " nv_savesize
/*[EXECUTABLE]*/                     " nv_NameCompCd: " nv_NameCompCd  FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" ";" STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                 SKIP.
/*[EXECUTABLE]*/                 OUTPUT CLOSE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1ChkVehicle C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1ChkVehicle :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       OUTPUT TO ConfirmBy.TXT.
/*[EXECUTABLE]*/                       PUT IntPol7072.ConfirmBy SKIP.
/*[EXECUTABLE]*/                       OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                       IF IntPol7072.ChkVehicle = YES THEN DO:
/*[EXECUTABLE]*/                         nv_resulttext = "Êè§µÃÇ¨ÊÀÒ¾Ã¶".
/*[EXECUTABLE]*/                         nv_cvehtext   = "µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)
/*[EXECUTABLE]*/                                       + " ¡ÃÁ¸ÃÃÁì¾Ãº.: "       + TRIM(nv_PolicyV72)
/*[EXECUTABLE]*/                                       +   " ·ÐàºÕÂ¹Ã¶ : "       + TRIM(IntPol7072.Registration) 
/*[EXECUTABLE]*/                                                     + " "       + TRIM(IntPol7072.RegisteredProvCd)
/*[EXECUTABLE]*/                                       + " ¼ÙéàÍÒ»ÃÐ¡Ñ¹ÀÑÂ : "   + TRIM(TRIM(TRIM(IntPol7072.InsuredTitle)
/*[EXECUTABLE]*/                                       + " " + TRIM(IntPol7072.InsuredName))
/*[EXECUTABLE]*/                                       + " " + TRIM(IntPol7072.InsuredSurname)).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         OUTPUT TO ChkVehicle.TXT.
/*[EXECUTABLE]*/                         PUT " ConfirmBy:" IntPol7072.ConfirmBy  SKIP.
/*[EXECUTABLE]*/                         PUT "ChkVehicle:" IntPol7072.ChkVehicle SKIP.
/*[EXECUTABLE]*/                         PUT "           " nv_resulttext FORMAT "X(50)" SKIP.
/*[EXECUTABLE]*/                         PUT "           " nv_cvehtext   FORMAT "X(200)" SKIP.
/*[EXECUTABLE]*/                         PUT "ChkVehSend:" IntPol7072.ChkVehSend SKIP.
/*[EXECUTABLE]*/                         PUT "   Mail to:" IntPol7072.ChkVehMail SKIP.
/*[EXECUTABLE]*/                         PUT "------------------------------------------------" FORMAT "X(50)" SKIP.
/*[EXECUTABLE]*/                         PUT "ChkVehAssignSend: " IntPol7072.ChkVehAssignSend SKIP.
/*[EXECUTABLE]*/                         PUT "ChkVehAssignMail: " IntPol7072.ChkVehAssignMail SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF IntPol7072.ChkVehSend = YES THEN DO:
/*[EXECUTABLE]*/                           IF IntPol7072.ChkVehMail <> "" THEN DO:
/*[EXECUTABLE]*/                             RUN GW/gwtomail 
/*[EXECUTABLE]*/                               (IntPol7072.ChkVehMail /*To "USERNAME"*/
/*[EXECUTABLE]*/                               ,""          /*CC.*/
/*[EXECUTABLE]*/                               ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)) /*Subject: WARNING VIB*/
/*[EXECUTABLE]*/                               ,nv_cvehtext /*Body*/
/*[EXECUTABLE]*/                               ).
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF IntPol7072.ChkVehAssignSend = YES THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           IF IntPol7072.ChkVehAssignMail <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                             RUN GW/gwtomail
/*[EXECUTABLE]*/                               (IntPol7072.ChkVehAssignMail
/*[EXECUTABLE]*/                               ,""
/*[EXECUTABLE]*/                               ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70))
/*[EXECUTABLE]*/                               ,nv_cvehtext
/*[EXECUTABLE]*/                               ).
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                         END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF IntPol7072.TransferToPremium = YES THEN DO: /* Gen Uwm100 to DB GWCtx */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                           RUN WRS\WRSGwCtx 
/*[EXECUTABLE]*/                               (IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                               ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                               ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                               ,0  /*RECID(uwm100)*/
/*[EXECUTABLE]*/                               ,IntPol7072.CompanyCode  /*833*/
/*[EXECUTABLE]*/                               ,IntPol7072.PolicyTypeCd /*2.2*/
/*[EXECUTABLE]*/                               ,IntPol7072.RateGroup).  /*110*/
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF IntS7072.ChkVehicle = YES*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt72 C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD1FileAtt72 :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt834 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1FileAtt834 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_COPYTOFILE  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LineSeqno   AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lwaitcount  AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LcountAgain AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lcount      AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_StartCount  AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LastCount   AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Acno1  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Agent  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errort AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrgName  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrmPrg   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_firstchk AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_verror   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 ASSIGN NV_Lwaitcount = 110000
/*[EXECUTABLE]*/                 NV_StartCount = 0
/*[EXECUTABLE]*/                 NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
/*[EXECUTABLE]*/                 FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[EXECUTABLE]*/                 nv_LineSeqno = 0.
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/ /* 2.1             110 */
/*[EXECUTABLE]*/                 IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN 
/*[EXECUTABLE]*/                     RUN PD_SAVEPD1FileAtt_1 (nv_RECIDIntPol7072
/*[EXECUTABLE]*/                                             ,nv_RECIDIntS7072).
/*[EXECUTABLE]*/                 ELSE DO:  /*case »¡µÔ add by kridtiya i. */ 
/*[EXECUTABLE]*/                 IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
/*[EXECUTABLE]*/                   RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                       (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                       ,"V70"
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                       ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                       ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                       ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[EXECUTABLE]*/                   ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[EXECUTABLE]*/                   RUN PD_FNameAttach
/*[EXECUTABLE]*/                       (INPUT IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,INPUT "V70"      /*v70,v72*/
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.PolicyTypeCd
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   IF    IntPol7072.PolicyTypeCd = "1"
/*[EXECUTABLE]*/                      OR IntPol7072.PolicyTypeCd = "2"
/*[EXECUTABLE]*/                      OR IntPol7072.PolicyTypeCd = "3"
/*[EXECUTABLE]*/                   THEN DO:
/*[EXECUTABLE]*/                       IF  (IntPol7072.PolicyTypeCd = "3"  AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )  /*TEST Prn cover 1*/
/*[EXECUTABLE]*/                          OR IntPol7072.CompanyCode  = "476" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
/*[EXECUTABLE]*/                       THEN DO:
/*[EXECUTABLE]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
/*[EXECUTABLE]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[EXECUTABLE]*/                         RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                           ,IntPol7072.PolicyNumber 
/*[EXECUTABLE]*/                           ,IntPol7072.Rencnt       
/*[EXECUTABLE]*/                           ,IntPol7072.Endcnt       
/*[EXECUTABLE]*/                           ,IntPol7072.DocumentUID  
/*[EXECUTABLE]*/                           ,IntPol7072.RqUID        /*nv_code keyRequestIndRq*/
/*[EXECUTABLE]*/                           ,""                      /*n_user  */
/*[EXECUTABLE]*/                           ,""                      /*n_passwd */
/*[EXECUTABLE]*/                           ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                         IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[COMMENT]*/                                /*RUN wctx\Wctxr702_RP.p*/
/*[EXECUTABLE]*/                             RUN Wctx/Wctxr701_1_CP.P
/*[EXECUTABLE]*/                                 (IntPol7072.CompanyCode  
/*[EXECUTABLE]*/                                  ,IntPol7072.PolicyNumber 
/*[EXECUTABLE]*/                                  ,IntPol7072.Rencnt       
/*[EXECUTABLE]*/                                  ,IntPol7072.Endcnt       
/*[EXECUTABLE]*/                                  ,IntPol7072.DocumentUID  
/*[EXECUTABLE]*/                                  ,IntPol7072.RqUID        
/*[EXECUTABLE]*/                                  ,""                      
/*[EXECUTABLE]*/                                  ,""                      
/*[EXECUTABLE]*/                                  ,OUTPUT nv_SAVEmsgerror).            
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:  /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ *//*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
/*[EXECUTABLE]*/                     IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[EXECUTABLE]*/                        OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" OR IntPol7072.CompanyCode = "570"
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" )             /* isuzu */
/*[EXECUTABLE]*/                        AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") ) THEN DO:  /* ·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
/*[EXECUTABLE]*/                       IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".  /*"Wctx/Wctxr703_1".*/
/*[EXECUTABLE]*/                                          ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
/*[EXECUTABLE]*/                       RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                          (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                          ,IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                          ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                          ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                          ,IntPol7072.DocumentUID 
/*[EXECUTABLE]*/                          ,IntPol7072.RqUID
/*[EXECUTABLE]*/                          ,""
/*[EXECUTABLE]*/                          ,""
/*[EXECUTABLE]*/                          ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                       IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[COMMENT]*/                              /*RUN wctx\Wctxr702_RP.p*/
/*[EXECUTABLE]*/                           RUN Wctx/Wctxr703_1ASN_RP.p
/*[EXECUTABLE]*/                               (IntPol7072.CompanyCode  
/*[EXECUTABLE]*/                               ,IntPol7072.PolicyNumber 
/*[EXECUTABLE]*/                               ,IntPol7072.Rencnt       
/*[EXECUTABLE]*/                               ,IntPol7072.Endcnt       
/*[EXECUTABLE]*/                               ,IntPol7072.DocumentUID  
/*[EXECUTABLE]*/                               ,IntPol7072.RqUID        
/*[EXECUTABLE]*/                               ,""                      
/*[EXECUTABLE]*/                               ,""                      
/*[EXECUTABLE]*/                               ,OUTPUT nv_SAVEmsgerror).            
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[EXECUTABLE]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   nv_firstchk   = YES.
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1ChkFile
/*[EXECUTABLE]*/                       (INPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT "V70"
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.PolicyTypeCd
/*[EXECUTABLE]*/                       ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[EXECUTABLE]*/                               + ".TXT". 
/*[EXECUTABLE]*/                     OUTPUT TO VALUE(nv_verror).
/*[EXECUTABLE]*/                     PUT 
/*[EXECUTABLE]*/                     "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                     " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[EXECUTABLE]*/                     " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     RETURN. 
/*[EXECUTABLE]*/                   END.
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
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = IntPol7072.PolicyTypeCd.
/*[EXECUTABLE]*/                   FOR EACH FNameAttach WHERE FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                      AND FNameAttach.PolicyTypeCd = "V70"
/*[EXECUTABLE]*/                      AND FNameAttach.CoverTypeCd  = nv_covcodtyp1  /*IntPol7072.PolicyTypeCd*/
/*[EXECUTABLE]*/                      AND FNameAttach.EffDate     <= TODAY  NO-LOCK
/*[EXECUTABLE]*/                   BREAK BY FNameAttach.SelectNumber:
/*[EXECUTABLE]*/                     IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                      ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[EXECUTABLE]*/                     IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[EXECUTABLE]*/                                          + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[EXECUTABLE]*/                       ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /*IF IntPol7072.CompanyCode = "469" THEN nv_COPYTOFILE = string(IntPol7072.DocumentUID) + "_" + nv_COPYTOFILE.*//*comment by Kridtiya i. 24/07/2017*/
/*[EXECUTABLE]*/                     nv_errortext = "".
/*[EXECUTABLE]*/                     IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     loop1:
/*[EXECUTABLE]*/                     REPEAT:
/*[EXECUTABLE]*/                       IF nv_firstchk = NO THEN DO:   
/*[EXECUTABLE]*/                         NV_Lcount = 0.
/*[EXECUTABLE]*/                         DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[EXECUTABLE]*/                           NV_Lcount = NV_Lcount + 1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       nv_firstchk = NO.
/*[EXECUTABLE]*/                       IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[EXECUTABLE]*/                         NV_StartCount = NV_StartCount + NV_Lcount.
/*[EXECUTABLE]*/                         IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[EXECUTABLE]*/                         NEXT loop1.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                             nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[EXECUTABLE]*/                       DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[EXECUTABLE]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT nv_RECIDIntPol7072 ).
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[EXECUTABLE]*/                         FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                         CREATE TFileAttach.
/*[EXECUTABLE]*/                         TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[EXECUTABLE]*/                         COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
/*[EXECUTABLE]*/                         IF ERROR-STATUS:ERROR  THEN DO:
/*[EXECUTABLE]*/                           nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[EXECUTABLE]*/                                        + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[EXECUTABLE]*/                         END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                         IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[EXECUTABLE]*/                         FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                           nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 1 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile1   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile1     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 2 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile2   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile2     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_errortext <> "" THEN
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[EXECUTABLE]*/                             IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[EXECUTABLE]*/                       LEAVE loop1.
/*[EXECUTABLE]*/                     END. /*loop1:*/
/*[EXECUTABLE]*/                   END. /*FOR EACH FNameAttach*/
/*[EXECUTABLE]*/                 END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /* ------------------------------------------------------------------------ */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
/*[EXECUTABLE]*/                 IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                   RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                       (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                       ,"V72"
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                       ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                       ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                       ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[EXECUTABLE]*/                   NV_StartCount = 0.
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[EXECUTABLE]*/                   ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[EXECUTABLE]*/                   RUN PD_FNameAttach
/*[EXECUTABLE]*/                      (INPUT        IntPol7072.CompanyCode
/*[EXECUTABLE]*/                      ,INPUT        "V72"      /*v70,v72*/
/*[EXECUTABLE]*/                      ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[COMMENT]*/                         /**/
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**//*Blank form Compulsory */
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode = "833" THEN DO:
/*[EXECUTABLE]*/                     RUN PD_ChkBlankForm
/*[EXECUTABLE]*/                          (INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_PrmPrg).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_PrgName = "" THEN DO:
/*[EXECUTABLE]*/                     RUN Wctx/wctxr702.P
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                        ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                        ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                        ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                        ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                        ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                        ,""         /*n_user   */
/*[EXECUTABLE]*/                        ,""         /*n_passwd */
/*[EXECUTABLE]*/                        ,nv_PrmPrg  /*Name Report*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[EXECUTABLE]*/                     nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[EXECUTABLE]*/                     RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                         (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                         ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                         ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                         ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                         ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                         ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                         ,""        /*n_user   */
/*[EXECUTABLE]*/                         ,""        /*n_passwd */
/*[EXECUTABLE]*/                         ,nv_PrmPrg /*Name Report="V72A4"*/
/*[EXECUTABLE]*/                         ,""        /*remark*/
/*[EXECUTABLE]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[EXECUTABLE]*/                       RUN wctx\Wctxr702_RP.p
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode       /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                        ,IntPol7072.CMIPolicyNumber   
/*[EXECUTABLE]*/                        ,IntPol7072.Rencnt            
/*[EXECUTABLE]*/                        ,IntPol7072.Endcnt            
/*[EXECUTABLE]*/                        ,IntPol7072.CMIDocumentUID    
/*[EXECUTABLE]*/                        ,IntPol7072.RqUID             /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                        , ""                          /*n_user   */
/*[EXECUTABLE]*/                        , ""                          /*n_passwd */
/*[EXECUTABLE]*/                        , ""                          /*Name Report*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_SAVEmsgerror).    
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN RETURN.     
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.  */                     
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1ChkFile
/*[EXECUTABLE]*/                       (INPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT "V72"
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.CMIPolicyTypeCd
/*[EXECUTABLE]*/                       ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[EXECUTABLE]*/                               + ".TXT".
/*[EXECUTABLE]*/                     OUTPUT TO VALUE(nv_verror).
/*[EXECUTABLE]*/                     PUT 
/*[EXECUTABLE]*/                     "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                     " Policy no.: "    IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[EXECUTABLE]*/                     " Contract no.: "  IntPol7072.ContractNumber FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     RETURN. 
/*[EXECUTABLE]*/                   END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[EXECUTABLE]*/                   ASSIGN nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[EXECUTABLE]*/                   FOR EACH FNameAttach WHERE
/*[EXECUTABLE]*/                            FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                        AND FNameAttach.PolicyTypeCd = "V72"
/*[EXECUTABLE]*/                        AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd  /*¾Ãº ËÃ×Í "T"*/
/*[EXECUTABLE]*/                        AND FNameAttach.EffDate     <= TODAY
/*[EXECUTABLE]*/                   NO-LOCK
/*[EXECUTABLE]*/                   BREAK BY FNameAttach.SelectNumber
/*[EXECUTABLE]*/                   :
/*[EXECUTABLE]*/                     IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                      ELSE nv_INPUTFileName = FNameAttach.CopyFileName.  /**/
/*[EXECUTABLE]*/                     IF FNameAttach.ToFileName   = "" THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[EXECUTABLE]*/                                          + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[EXECUTABLE]*/                       ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     nv_errortext = "".
/*[EXECUTABLE]*/                     IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[EXECUTABLE]*/                     IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
/*[EXECUTABLE]*/                             nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     loop2:
/*[EXECUTABLE]*/                     REPEAT:
/*[EXECUTABLE]*/                       IF nv_firstchk = NO THEN DO:   
/*[EXECUTABLE]*/                         NV_Lcount = 0.
/*[EXECUTABLE]*/                         DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[EXECUTABLE]*/                           NV_Lcount = NV_Lcount + 1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       nv_firstchk = NO.
/*[EXECUTABLE]*/                       IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[EXECUTABLE]*/                         NV_StartCount = NV_StartCount + NV_Lcount.
/*[EXECUTABLE]*/                         IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
/*[EXECUTABLE]*/                         NEXT loop2.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                       RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT nv_RECIDIntPol7072 ).
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[EXECUTABLE]*/                         FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                         CREATE TFileAttach.
/*[EXECUTABLE]*/                         TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[EXECUTABLE]*/                         COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[EXECUTABLE]*/                         IF ERROR-STATUS:ERROR  THEN DO:
/*[EXECUTABLE]*/                           nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[EXECUTABLE]*/                                        + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
/*[EXECUTABLE]*/                         FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                           nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 1 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile1   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile1     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 2 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile2   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile2     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_errortext <> "" THEN
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[EXECUTABLE]*/                             IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       LEAVE loop2.
/*[EXECUTABLE]*/                     END. /*loop2:*/
/*[EXECUTABLE]*/                   END. /*FOR EACH FNameAttach*/
/*[EXECUTABLE]*/                 END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[EXECUTABLE]*/                 END. /*end kridtiya i.7072*/  /*..............*/
/*[EXECUTABLE]*/                 FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                 RELEASE FNameAttach.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAttach C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1FileAttach :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_COPYTOFILE  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LineSeqno   AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lwaitcount  AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LcountAgain AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lcount      AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_StartCount  AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LastCount   AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Acno1  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_Agent  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errort AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrgName  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrmPrg   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_firstchk AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_verror   AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 ASSIGN NV_Lwaitcount = 110000
/*[EXECUTABLE]*/                 NV_StartCount = 0
/*[EXECUTABLE]*/                 NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
/*[EXECUTABLE]*/                 FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[EXECUTABLE]*/                 nv_LineSeqno = 0.
/*[COMMENT]*/                    /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/ /* 2.1             110 */
/*[EXECUTABLE]*/                 IF (IntPol7072.PolicyTypeCd <> "" ) AND (IntPol7072.CMIPolicyTypeCd <> "") THEN
/*[EXECUTABLE]*/                     RUN PD_SAVEPD1FileAtt_1 (nv_RECIDIntPol7072
/*[EXECUTABLE]*/                                             ,nv_RECIDIntS7072).
/*[EXECUTABLE]*/                 ELSE DO:  /*case »¡µÔ add by kridtiya i. */ 
/*[EXECUTABLE]*/                 IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
/*[EXECUTABLE]*/                   RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                       (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                       ,"V70"
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                       ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                       ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                       ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[EXECUTABLE]*/                   ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[EXECUTABLE]*/                   RUN PD_FNameAttach
/*[EXECUTABLE]*/                       (INPUT IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,INPUT "V70"      /*v70,v72*/
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.PolicyTypeCd
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrmPrg ).
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  = "476" THEN 
/*[EXECUTABLE]*/                     RUN PD_SAVEPD1F_PrgName (INPUT nv_RECIDIntPol7072
/*[EXECUTABLE]*/                                             ,INPUT-OUTPUT nv_PrgName).
/*[EXECUTABLE]*/                   IF    IntPol7072.PolicyTypeCd = "1" OR IntPol7072.PolicyTypeCd = "2" OR IntPol7072.PolicyTypeCd = "3" THEN DO:
/*[COMMENT]*/                        /* Add by Kridtiya i...Lockton */
/*[EXECUTABLE]*/                     IF IntPol7072.CompanyCode = "469" THEN DO:   
/*[EXECUTABLE]*/                       IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1". /*"Wctx/Wctxr708".*/
/*[EXECUTABLE]*/                       ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr708.p Form 1 Policy*/
/*[EXECUTABLE]*/                       RUN VALUE(nv_PrgName)        
/*[EXECUTABLE]*/                          (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                          ,IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                          ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                          ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                          ,IntPol7072.DocumentUID
/*[EXECUTABLE]*/                           ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                          ,""               /*n_user   */
/*[EXECUTABLE]*/                          ,""               /*n_passwd */
/*[EXECUTABLE]*/                          ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                     END. /*End add Kridtiya i. */
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF  (IntPol7072.PolicyTypeCd = "3"  AND substr(IntPol7072.PolicyNumber,1,1) <> "R" AND substr(IntPol7072.PolicyNumber,1,1) <> "Q" ) OR  /*kridtiya i.*/
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )  /*TEST Prn cover 1*/
/*[EXECUTABLE]*/                          OR IntPol7072.CompanyCode  = "476" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
/*[EXECUTABLE]*/                       THEN DO:
/*[EXECUTABLE]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
/*[EXECUTABLE]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[EXECUTABLE]*/                         RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                           ,IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                           ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                           ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                           ,IntPol7072.DocumentUID
/*[EXECUTABLE]*/                           ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
/*[EXECUTABLE]*/                           ,""  /*n_user  */
/*[EXECUTABLE]*/                           ,""  /*n_passwd */
/*[EXECUTABLE]*/                           ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:  /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ *//*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
/*[EXECUTABLE]*/                     IF (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND 
/*[EXECUTABLE]*/                         substr(IntPol7072.PolicyNumber,1,1) <> "R" AND substr(IntPol7072.PolicyNumber,1,1) <> "Q" )
/*[EXECUTABLE]*/                        OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834"  OR IntPol7072.CompanyCode = "570" OR IntPol7072.CompanyCode = "1012" 
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"  OR IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "1098" 
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "476" OR IntPol7072.CompanyCode = "210"  OR IntPol7072.CompanyCode = "M82" OR IntPol7072.CompanyCode = "1103" 
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "M73" OR IntPol7072.CompanyCode = "M85"  OR IntPol7072.CompanyCode = "839" OR IntPol7072.CompanyCode = "1107"
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "1018" OR    IntPol7072.CompanyCode = "1470" OR IntPol7072.CompanyCode = "1752"
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "444" OR IntPol7072.CompanyCode = "1056" OR IntPol7072.CompanyCode = "1141" OR IntPol7072.CompanyCode = "1146"
/*[EXECUTABLE]*/                        OR    IntPol7072.CompanyCode = "1554" OR    IntPol7072.CompanyCode = "1798" OR    IntPol7072.CompanyCode = "2117" )
/*[EXECUTABLE]*/                        AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") ) THEN DO:  /* ·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
/*[EXECUTABLE]*/                       IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".     /*"Wctx/Wctxr703_1".*/
/*[EXECUTABLE]*/                                          ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr703A4*/
/*[EXECUTABLE]*/                       RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                          (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                          ,IntPol7072.PolicyNumber
/*[EXECUTABLE]*/                          ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                          ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                          ,IntPol7072.DocumentUID 
/*[EXECUTABLE]*/                          ,IntPol7072.RqUID
/*[EXECUTABLE]*/                          ,""
/*[EXECUTABLE]*/                          ,""
/*[EXECUTABLE]*/                          ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN  RETURN.
/*[EXECUTABLE]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   nv_firstchk   = YES.
/*[COMMENT]*/                      /*PAUSE 1 NO-MESSAGE.*/
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1ChkFile
/*[EXECUTABLE]*/                       (INPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT "V70"
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.PolicyTypeCd
/*[EXECUTABLE]*/                       ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[EXECUTABLE]*/                               + ".TXT". 
/*[EXECUTABLE]*/                     OUTPUT TO VALUE(nv_verror).
/*[EXECUTABLE]*/                     PUT "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                     " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[EXECUTABLE]*/                     " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     RETURN. 
/*[EXECUTABLE]*/                   END.
/*[COMMENT]*/                      /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = IntPol7072.PolicyTypeCd.
/*[EXECUTABLE]*/                   FOR EACH FNameAttach WHERE FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                      AND FNameAttach.PolicyTypeCd = "V70"
/*[EXECUTABLE]*/                      AND FNameAttach.CoverTypeCd  = nv_covcodtyp1  /*IntPol7072.PolicyTypeCd*/
/*[EXECUTABLE]*/                      /*AND FNameAttach.EffDate     <= TODAY*/     NO-LOCK
/*[EXECUTABLE]*/                   BREAK BY FNameAttach.SelectNumber:
/*[EXECUTABLE]*/                     IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                      ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[EXECUTABLE]*/                     IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[EXECUTABLE]*/                                          + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[EXECUTABLE]*/                       ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /*IF IntPol7072.CompanyCode = "469" THEN nv_COPYTOFILE = string(IntPol7072.DocumentUID) + "_" + nv_COPYTOFILE.*//*comment by Kridtiya i. 24/07/2017*/
/*[EXECUTABLE]*/                     nv_errortext = "".
/*[EXECUTABLE]*/                     IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[EXECUTABLE]*/                     loop1:
/*[EXECUTABLE]*/                     REPEAT:
/*[EXECUTABLE]*/                       IF nv_firstchk = NO THEN DO:   
/*[EXECUTABLE]*/                         NV_Lcount = 0.
/*[EXECUTABLE]*/                         DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[EXECUTABLE]*/                           NV_Lcount = NV_Lcount + 1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       nv_firstchk = NO.
/*[EXECUTABLE]*/                       IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[EXECUTABLE]*/                         NV_StartCount = NV_StartCount + NV_Lcount.
/*[EXECUTABLE]*/                         IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[EXECUTABLE]*/                         NEXT loop1.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                             nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[EXECUTABLE]*/                       DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[EXECUTABLE]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT RECID(IntPol7072) ).
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[EXECUTABLE]*/                         FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                         CREATE TFileAttach.
/*[EXECUTABLE]*/                         TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[EXECUTABLE]*/                         COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
/*[EXECUTABLE]*/                         IF ERROR-STATUS:ERROR  THEN DO:
/*[EXECUTABLE]*/                           nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[EXECUTABLE]*/                                        + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[EXECUTABLE]*/                         END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                         IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[EXECUTABLE]*/                         FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                           nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 1 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile1   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile1     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 2 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile2   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile2     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_errortext <> "" THEN
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[EXECUTABLE]*/                             IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[EXECUTABLE]*/                       LEAVE loop1.
/*[EXECUTABLE]*/                     END. /*loop1:*/
/*[EXECUTABLE]*/                   END. /*FOR EACH FNameAttach*/
/*[EXECUTABLE]*/                 END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[COMMENT]*/                    /* ------------------------------------------------------------------------ */
/*[COMMENT]*/                    /* ¾Ãº. / Compulsory */
/*[EXECUTABLE]*/                 IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                   RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                       (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                       ,"V72"
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                       ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                       ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                       ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                       ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[EXECUTABLE]*/                   NV_StartCount = 0.
/*[COMMENT]*/                      /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[EXECUTABLE]*/                   ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[EXECUTABLE]*/                   RUN PD_FNameAttach
/*[EXECUTABLE]*/                      (INPUT        IntPol7072.CompanyCode
/*[EXECUTABLE]*/                      ,INPUT        "V72"      /*v70,v72*/
/*[EXECUTABLE]*/                      ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                      ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**//*Blank form Compulsory */
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode = "833" THEN DO:
/*[EXECUTABLE]*/                     RUN PD_ChkBlankForm
/*[EXECUTABLE]*/                          (INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_PrmPrg).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_PrgName = "" THEN DO:
/*[EXECUTABLE]*/                     RUN Wctx/wctxr702.P
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                        ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                        ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                        ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                        ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                        ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                        ,""         /*n_user   */
/*[EXECUTABLE]*/                        ,""         /*n_passwd */
/*[EXECUTABLE]*/                        ,nv_PrmPrg  /*Name Report*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[EXECUTABLE]*/                     nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                        /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[EXECUTABLE]*/                     RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                         (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                         ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                         ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                         ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                         ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                         ,IntPol7072.CMIBarCodeNumber
/*[EXECUTABLE]*/                         ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                         ,""        /*n_user   */
/*[EXECUTABLE]*/                         ,""        /*n_passwd */
/*[EXECUTABLE]*/                         ,nv_PrmPrg /*Name Report="V72A4"*/
/*[EXECUTABLE]*/                         ,""        /*remark*/
/*[EXECUTABLE]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  = "834" AND IntPol7072.SERVICE_ID = "online" THEN DO: /*Add by Kridtiya i.834*/ 
/*[EXECUTABLE]*/                       RUN wctx\Wctxr702_RP.p
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode       /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                        ,IntPol7072.CMIPolicyNumber   
/*[EXECUTABLE]*/                        ,IntPol7072.Rencnt            
/*[EXECUTABLE]*/                        ,IntPol7072.Endcnt            
/*[EXECUTABLE]*/                        ,IntPol7072.CMIDocumentUID    
/*[EXECUTABLE]*/                        ,IntPol7072.RqUID             /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                        , ""                          /*n_user   */
/*[EXECUTABLE]*/                        , ""                          /*n_passwd */
/*[EXECUTABLE]*/                        , ""                          /*Name Report*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_SAVEmsgerror).    
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN RETURN.     
/*[EXECUTABLE]*/                   PAUSE 1 NO-MESSAGE.                       
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1ChkFile
/*[EXECUTABLE]*/                       (INPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT "V72"
/*[EXECUTABLE]*/                       ,INPUT IntPol7072.CMIPolicyTypeCd
/*[EXECUTABLE]*/                       ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[EXECUTABLE]*/                               + ".TXT".
/*[EXECUTABLE]*/                     OUTPUT TO VALUE(nv_verror).
/*[EXECUTABLE]*/                     PUT "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                     " Policy no.: "    IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[EXECUTABLE]*/                     " Contract no.: "  IntPol7072.ContractNumber FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     RETURN. 
/*[EXECUTABLE]*/                   END.
/*[COMMENT]*/                      /* -------------------------------------------------------------------------------- */
/*[EXECUTABLE]*/                   ASSIGN nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[EXECUTABLE]*/                   FOR EACH FNameAttach WHERE
/*[EXECUTABLE]*/                            FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                        AND FNameAttach.PolicyTypeCd = "V72"
/*[EXECUTABLE]*/                        AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd  /*¾Ãº ËÃ×Í "T"*/
/*[COMMENT]*/                           /*AND FNameAttach.EffDate     <= TODAY*/
/*[EXECUTABLE]*/                   NO-LOCK
/*[EXECUTABLE]*/                   BREAK BY FNameAttach.SelectNumber  :
/*[EXECUTABLE]*/                     IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                      ELSE nv_INPUTFileName = FNameAttach.CopyFileName.  /**/
/*[EXECUTABLE]*/                     IF FNameAttach.ToFileName   = "" THEN nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[EXECUTABLE]*/                                          + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[EXECUTABLE]*/                       ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     nv_errortext = "".
/*[EXECUTABLE]*/                     IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[EXECUTABLE]*/                     IF TRIM(nv_COPYTOFILE) = ".PDF" THEN nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     loop2:
/*[EXECUTABLE]*/                     REPEAT:
/*[EXECUTABLE]*/                       IF nv_firstchk = NO THEN DO:   
/*[EXECUTABLE]*/                         NV_Lcount = 0.
/*[EXECUTABLE]*/                         DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[EXECUTABLE]*/                           NV_Lcount = NV_Lcount + 1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       nv_firstchk = NO.
/*[EXECUTABLE]*/                       IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[EXECUTABLE]*/                         NV_StartCount = NV_StartCount + NV_Lcount.
/*[EXECUTABLE]*/                         IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
/*[EXECUTABLE]*/                         NEXT loop2.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                       RUN Pd_saveCer (INPUT-OUTPUT nv_COPYTOFILE ,INPUT RECID(IntPol7072) ).
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[EXECUTABLE]*/                         FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                         CREATE TFileAttach.
/*[EXECUTABLE]*/                         TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[EXECUTABLE]*/                         COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[EXECUTABLE]*/                         IF ERROR-STATUS:ERROR  THEN DO:
/*[EXECUTABLE]*/                           nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[EXECUTABLE]*/                                        + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[EXECUTABLE]*/                         FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072  NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                           nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 1 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile1   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile1     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 2 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile2   = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile2     = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_errortext <> "" THEN
/*[EXECUTABLE]*/                             ASSIGN IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[EXECUTABLE]*/                             IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       LEAVE loop2.
/*[EXECUTABLE]*/                     END. /*loop2:*/
/*[EXECUTABLE]*/                   END. /*FOR EACH FNameAttach*/
/*[EXECUTABLE]*/                 END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[EXECUTABLE]*/                 END. /*end kridtiya i.7072*/  /*..............*/
/*[EXECUTABLE]*/                 FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                 RELEASE FNameAttach.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAttach2 C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD1FileAttach2 :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   ASSIGN
/*[UNCALLED]*/                   NV_Lwaitcount = 110000
/*[UNCALLED]*/                   NV_StartCount = 0
/*[UNCALLED]*/                   NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
/*[UNCALLED]*/                   FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[UNCALLED]*/                   nv_LineSeqno = 0.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/
/*[UNCALLED]*/                      /* 2.1                             110 */
/*[UNCALLED]*/                   IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
/*[UNCALLED]*/                     RUN WSP/WSPMCpny.P
/*[UNCALLED]*/                          (IntPol7072.CompanyCode
/*[UNCALLED]*/                          ,IntPol7072.BranchCd
/*[UNCALLED]*/                          ,"V70"
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerBranch 
/*[UNCALLED]*/                          ,OUTPUT nv_Acno1
/*[UNCALLED]*/                          ,OUTPUT nv_Agent
/*[UNCALLED]*/                          ,OUTPUT nv_errort).
/*[UNCALLED]*/                     IF nv_errort <> "" THEN RETURN.
/*[UNCALLED]*/                     /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                     ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[UNCALLED]*/                     RUN PD_FNameAttach
/*[UNCALLED]*/                         (INPUT IntPol7072.CompanyCode
/*[UNCALLED]*/                         ,INPUT "V70"      /*v70,v72*/
/*[UNCALLED]*/                         ,INPUT IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                         /**/
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrmPrg ).
/*[UNCALLED]*/                     /**/
/*[UNCALLED]*/                     IF    IntPol7072.PolicyTypeCd = "1"
/*[UNCALLED]*/                        OR IntPol7072.PolicyTypeCd = "2"
/*[UNCALLED]*/                        OR IntPol7072.PolicyTypeCd = "3"
/*[UNCALLED]*/                     THEN DO:
/*[UNCALLED]*/                         IF  (IntPol7072.PolicyTypeCd = "3"   AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
/*[UNCALLED]*/                             /*TEST Prn cover 1*/
/*[UNCALLED]*/                            (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )
/*[UNCALLED]*/                             /*Isuzu */
/*[UNCALLED]*/                         /*OR (IntPol7072.CompanyCode  = "476" AND IntPol7072.PolicyTypeCd = "1" )*/
/*[UNCALLED]*/                         OR (IntPol7072.CompanyCode  = "476" ) OR (IntPol7072.CompanyCode  = "839") /*Add ART*/
/*[UNCALLED]*/                         THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
/*[UNCALLED]*/                                              ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                              (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
/*[UNCALLED]*/                              ,IntPol7072.PolicyNumber
/*[UNCALLED]*/                              ,IntPol7072.Rencnt
/*[UNCALLED]*/                              ,IntPol7072.Endcnt
/*[UNCALLED]*/                              ,IntPol7072.DocumentUID
/*[UNCALLED]*/                              ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
/*[UNCALLED]*/                              ,""               /*n_user  */
/*[UNCALLED]*/                              ,""               /*n_passwd */
/*[UNCALLED]*/                              ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                       /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
/*[UNCALLED]*/                       IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[UNCALLED]*/                          OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" OR IntPol7072.CompanyCode  = "839" /*Add ART*/
/*[UNCALLED]*/                          OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"
/*[UNCALLED]*/                          OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" ) /*isuzu*/
/*[UNCALLED]*/                              /**/
/*[UNCALLED]*/                          AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") )
/*[UNCALLED]*/                               /*·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
/*[UNCALLED]*/                       THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".  /*"Wctx/Wctxr703_1".*/
/*[UNCALLED]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                             (IntPol7072.CompanyCode
/*[UNCALLED]*/                             ,IntPol7072.PolicyNumber
/*[UNCALLED]*/                             ,IntPol7072.Rencnt
/*[UNCALLED]*/                             ,IntPol7072.Endcnt
/*[UNCALLED]*/                             ,IntPol7072.DocumentUID 
/*[UNCALLED]*/                             ,IntPol7072.RqUID
/*[UNCALLED]*/                             ,""
/*[UNCALLED]*/                             ,""
/*[UNCALLED]*/                             ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[UNCALLED]*/                     /* ---------------------------------------------------- */
/*[UNCALLED]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[UNCALLED]*/                     NO-ERROR NO-WAIT.
/*[UNCALLED]*/                     nv_firstchk   = YES.
/*[UNCALLED]*/                     /*PAUSE 1 NO-MESSAGE.*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     RUN PD_SAVEPD1ChkFile
/*[UNCALLED]*/                         (INPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT "V70"
/*[UNCALLED]*/                         ,INPUT IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN DO:
/*[UNCALLED]*/                       nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[UNCALLED]*/                                 + ".TXT".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       OUTPUT TO VALUE(nv_verror).
/*[UNCALLED]*/                       PUT 
/*[UNCALLED]*/                       "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[UNCALLED]*/                       " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[UNCALLED]*/                       " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                       SKIP.
/*[UNCALLED]*/                       OUTPUT CLOSE.
/*[UNCALLED]*/                       RETURN. 
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[UNCALLED]*/                     /* -------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "3. SvFile:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     FOR EACH FNameAttach WHERE
/*[UNCALLED]*/                              FNameAttach.CompanyCode  = nv_NameCompCd
/*[UNCALLED]*/                          AND FNameAttach.PolicyTypeCd = "V70"
/*[UNCALLED]*/                          AND FNameAttach.CoverTypeCd  = IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                          AND FNameAttach.EffDate     <= TODAY
/*[UNCALLED]*/                     NO-LOCK
/*[UNCALLED]*/                     BREAK BY FNameAttach.SelectNumber:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[UNCALLED]*/                                                        ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       IF FNameAttach.ToFileName   = "" THEN
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[UNCALLED]*/                   
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[UNCALLED]*/                                            + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       nv_errortext = "".
/*[UNCALLED]*/                       IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       loop1:
/*[UNCALLED]*/                       REPEAT:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         IF nv_firstchk = NO THEN DO:   
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         nv_firstchk = NO.
/*[UNCALLED]*/                         IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[UNCALLED]*/                           NV_StartCount = NV_StartCount + NV_Lcount.
/*[UNCALLED]*/                           IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[UNCALLED]*/                           NEXT loop1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[UNCALLED]*/                         nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[UNCALLED]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                         /*
/*[UNCALLED]*/                         DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                           /*
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= 1000000:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END. */
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           CREATE TFileAttach.
/*[UNCALLED]*/                           TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[UNCALLED]*/                           /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[UNCALLED]*/                     
/*[UNCALLED]*/                           COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[UNCALLED]*/                           /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[UNCALLED]*/                           OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[UNCALLED]*/                             EXPORT TFileAttach.FileBinary.
/*[UNCALLED]*/                           OUTPUT CLOSE. */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                           IF ERROR-STATUS:ERROR  THEN DO:
/*[UNCALLED]*/                             nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[UNCALLED]*/                                          + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[UNCALLED]*/                           IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                           IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[UNCALLED]*/                           /**/
/*[UNCALLED]*/                           FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[UNCALLED]*/                           NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE IntS7072 THEN DO:
/*[UNCALLED]*/                             nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[UNCALLED]*/                             IF nv_LineSeqno = 1 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_LineSeqno = 2 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_errortext <> "" THEN
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[UNCALLED]*/                               IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         LEAVE loop1.
/*[UNCALLED]*/                       END. /*loop1:*/
/*[UNCALLED]*/                     END. /*FOR EACH FNameAttach*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[UNCALLED]*/                   /*  -------------------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /* ¾Ãº. / Compulsory */
/*[UNCALLED]*/                   IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     RUN WSP/WSPMCpny.P
/*[UNCALLED]*/                          (IntPol7072.CompanyCode
/*[UNCALLED]*/                          ,IntPol7072.BranchCd
/*[UNCALLED]*/                          ,"V72"
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerBranch 
/*[UNCALLED]*/                          ,OUTPUT nv_Acno1
/*[UNCALLED]*/                          ,OUTPUT nv_Agent
/*[UNCALLED]*/                          ,OUTPUT nv_errort).
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     IF nv_errort <> "" THEN RETURN.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     NV_StartCount = 0.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     /* ---------------------------------------------------- */
/*[UNCALLED]*/                     /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                     ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[UNCALLED]*/                     RUN PD_FNameAttach
/*[UNCALLED]*/                         (INPUT        IntPol7072.CompanyCode
/*[UNCALLED]*/                         ,INPUT        "V72"      /*v70,v72*/
/*[UNCALLED]*/                         ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[UNCALLED]*/                         /**/
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrmPrg ).
/*[UNCALLED]*/                     /**/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     /*Blank form Compulsory */
/*[UNCALLED]*/                     IF IntPol7072.CompanyCode = "833" THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       RUN PD_ChkBlankForm
/*[UNCALLED]*/                            (INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_PrmPrg).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     IF nv_PrgName = "" THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       RUN Wctx/wctxr702.P
/*[UNCALLED]*/                            (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[UNCALLED]*/                            ,IntPol7072.CMIPolicyNumber
/*[UNCALLED]*/                            ,IntPol7072.Rencnt
/*[UNCALLED]*/                            ,IntPol7072.Endcnt
/*[UNCALLED]*/                            ,IntPol7072.CMIDocumentUID
/*[UNCALLED]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                            ,""         /*n_user   */
/*[UNCALLED]*/                            ,""         /*n_passwd */
/*[UNCALLED]*/                            ,nv_PrmPrg  /*Name Report*/
/*[UNCALLED]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       nv_PrgName = "Wctx/" + nv_PrgName.
/*[UNCALLED]*/                       /*
/*[UNCALLED]*/                       RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[UNCALLED]*/                       RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                            (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[UNCALLED]*/                            ,IntPol7072.CMIPolicyNumber
/*[UNCALLED]*/                            ,IntPol7072.Rencnt
/*[UNCALLED]*/                            ,IntPol7072.Endcnt
/*[UNCALLED]*/                            ,IntPol7072.CMIDocumentUID
/*[UNCALLED]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                            ,""         /*n_user   */
/*[UNCALLED]*/                            ,""         /*n_passwd */
/*[UNCALLED]*/                            ,nv_PrmPrg  /*Name Report="V72A4"*/
/*[UNCALLED]*/                            ,""         /*remark*/
/*[UNCALLED]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "2. ChkPDF:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     /*PAUSE 1 NO-MESSAGE.*/
/*[UNCALLED]*/                     RUN PD_SAVEPD1ChkFile
/*[UNCALLED]*/                         (INPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT "V72"
/*[UNCALLED]*/                         ,INPUT IntPol7072.CMIPolicyTypeCd
/*[UNCALLED]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN DO:
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[UNCALLED]*/                                 + ".TXT".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       OUTPUT TO VALUE(nv_verror).
/*[UNCALLED]*/                       PUT 
/*[UNCALLED]*/                       "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[UNCALLED]*/                       " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[UNCALLED]*/                       " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                       SKIP.
/*[UNCALLED]*/                       OUTPUT CLOSE.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       RETURN. 
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     /* -------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "3. SvFile:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     ASSIGN
/*[UNCALLED]*/                     nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     FOR EACH FNameAttach WHERE
/*[UNCALLED]*/                              FNameAttach.CompanyCode  = nv_NameCompCd
/*[UNCALLED]*/                          AND FNameAttach.PolicyTypeCd = "V72"
/*[UNCALLED]*/                          AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
/*[UNCALLED]*/                          AND FNameAttach.EffDate     <= TODAY
/*[UNCALLED]*/                     NO-LOCK
/*[UNCALLED]*/                     BREAK BY FNameAttach.SelectNumber
/*[UNCALLED]*/                     :
/*[UNCALLED]*/                       IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[UNCALLED]*/                                                        ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       IF FNameAttach.ToFileName   = "" THEN
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[UNCALLED]*/                   
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[UNCALLED]*/                                            + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       nv_errortext = "".
/*[UNCALLED]*/                       IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
/*[UNCALLED]*/                               nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       loop2:
/*[UNCALLED]*/                       REPEAT:
/*[UNCALLED]*/                         IF nv_firstchk = NO THEN DO:   
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         nv_firstchk = NO.
/*[UNCALLED]*/                         IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[UNCALLED]*/                           NV_StartCount = NV_StartCount + NV_Lcount.
/*[UNCALLED]*/                           IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
/*[UNCALLED]*/                           NEXT loop2.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                         /*
/*[UNCALLED]*/                         DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[UNCALLED]*/                           FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                           CREATE TFileAttach.
/*[UNCALLED]*/                           TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[UNCALLED]*/                           COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[UNCALLED]*/                           IF ERROR-STATUS:ERROR  THEN DO:
/*[UNCALLED]*/                             nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[UNCALLED]*/                                          + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[UNCALLED]*/                           /**/
/*[UNCALLED]*/                           FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[UNCALLED]*/                           NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE IntS7072 THEN DO:
/*[UNCALLED]*/                             nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
/*[UNCALLED]*/                             IF nv_LineSeqno = 1 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_LineSeqno = 2 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_errortext <> "" THEN
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[UNCALLED]*/                               IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         LEAVE loop2.
/*[UNCALLED]*/                       END. /*loop2:*/
/*[UNCALLED]*/                     END. /*FOR EACH FNameAttach*/
/*[UNCALLED]*/                   END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   RELEASE FNameAttach.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt_1 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1FileAtt_1 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO. 
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO. 
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO. 
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO. 
/*[EXECUTABLE]*/                 DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO. 
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.*/
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
/*[COMMENT]*/                          /*Add kridtiya i. Date. 20/09/2016*/
/*[EXECUTABLE]*/                 IF   substr(IntPol7072.PolicyNumber,1,1) = "R" OR  substr(IntPol7072.PolicyNumber,1,1) = "Q"  THEN DO:  /* CMI*/
/*[COMMENT]*/                        /* by Kridtiya i. 20/09/2016 */
/*[EXECUTABLE]*/                     RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                        ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                        ,"V70"
/*[EXECUTABLE]*/                        ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                        ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                        ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                        ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[EXECUTABLE]*/                   IF nv_PrgName = "" THEN DO:
/*[EXECUTABLE]*/                       RUN Wctx/wctxr702.P
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                            ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                            ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                            ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                            ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                            ,""         /*n_user   */
/*[EXECUTABLE]*/                            ,""         /*n_passwd */
/*[EXECUTABLE]*/                            ,nv_PrmPrg  /*Name Report*/
/*[EXECUTABLE]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[EXECUTABLE]*/                       nv_PrgName = "Wctx/" + nv_PrgName.
/*[COMMENT]*/                          /*RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[EXECUTABLE]*/                       RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                           (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[EXECUTABLE]*/                            ,IntPol7072.CMIPolicyNumber
/*[EXECUTABLE]*/                            ,IntPol7072.Rencnt
/*[EXECUTABLE]*/                            ,IntPol7072.Endcnt
/*[EXECUTABLE]*/                            ,IntPol7072.CMIDocumentUID
/*[EXECUTABLE]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[EXECUTABLE]*/                            ,""        /*n_user   */
/*[EXECUTABLE]*/                            ,""        /*n_passwd */
/*[EXECUTABLE]*/                            ,nv_PrmPrg /*Name Report="V72A4"*/
/*[EXECUTABLE]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   RUN WSP/WSPMCpny.P
/*[EXECUTABLE]*/                        (IntPol7072.CompanyCode
/*[EXECUTABLE]*/                        ,IntPol7072.BranchCd
/*[EXECUTABLE]*/                        ,"V70"
/*[EXECUTABLE]*/                        ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[EXECUTABLE]*/                        ,OUTPUT nv_BrokerBranch 
/*[EXECUTABLE]*/                        ,OUTPUT nv_Acno1
/*[EXECUTABLE]*/                        ,OUTPUT nv_Agent
/*[EXECUTABLE]*/                        ,OUTPUT nv_errort).
/*[EXECUTABLE]*/                   IF nv_errort <> "" THEN RETURN.
/*[COMMENT]*/                      /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[EXECUTABLE]*/                   ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[EXECUTABLE]*/                   RUN PD_FNameAttach
/*[EXECUTABLE]*/                       (INPUT IntPol7072.CompanyCode
/*[EXECUTABLE]*/                       ,INPUT "V70"      /*v70,v72*/
/*[EXECUTABLE]*/                       ,INPUT (IntPol7072.PolicyTypeCd + "T")
/*[COMMENT]*/                          /**/
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrgName
/*[EXECUTABLE]*/                       ,INPUT-OUTPUT nv_PrmPrg ).
/*[COMMENT]*/                      /**/
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  = "476" THEN 
/*[EXECUTABLE]*/                             RUN PD_SAVEPD1F_PrgName (INPUT nv_RECIDIntPol7072
/*[EXECUTABLE]*/                                                     ,INPUT-OUTPUT nv_PrgName).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF    IntPol7072.PolicyTypeCd = "1" OR IntPol7072.PolicyTypeCd = "2" OR IntPol7072.PolicyTypeCd = "3" THEN DO:
/*[EXECUTABLE]*/                       DEFINE VARIABLE nv_code       AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                       DEFINE VARIABLE n_err         AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                           nv_code      = STRING(TODAY,"99/99/9999")
/*[EXECUTABLE]*/                                        + STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                                        + SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                           n_err        = ""
/*[EXECUTABLE]*/                           nv_PrmPrg    = "V70V72A4Receipt".
/*[BLANK]*/                           
/*[EXECUTABLE]*/                        IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr707A4_3P1".
/*[EXECUTABLE]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[COMMENT]*/                                                    /*
/*[COMMENT]*/                           IF (IntPol7072.RateGroup = "420" AND IntPol7072.GrossVehOrCombinedWeight <= "3" ) THEN nv_PrgName = "Wctx/" + "wctxizu706".
/*[COMMENT]*/                           ELSE IF (IntPol7072.RateGroup = "110" AND IntPol7072.SeatingCapacity  <= "15" )   THEN nv_PrgName = "Wctx/" + "wctxizu706".*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                                              (IntPol7072.CompanyCode                    
/*[EXECUTABLE]*/                                              ,IntPol7072.PolicyNumber                 
/*[EXECUTABLE]*/                                              ,IntPol7072.Rencnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.Endcnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.DocumentUID                  
/*[EXECUTABLE]*/                                              ,IntPol7072.RqUID                                
/*[EXECUTABLE]*/                                              ,""                                       
/*[EXECUTABLE]*/                                              ,""                             
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIPolicyNumber               
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIDocumentUID                
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
/*[EXECUTABLE]*/                                              ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                   ELSE DO:
/*[COMMENT]*/                        /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[COMMENT]*/                        /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐÀÑÂ + ¾Ãº.  » 2.1 3.1 */
/*[EXECUTABLE]*/                     IF  (IntPol7072.PolicyTypeCd = "2.1") OR (IntPol7072.PolicyTypeCd = "3.1")  THEN DO:
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr703_1".
/*[EXECUTABLE]*/                                          ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr709A4_3P2".
/*[EXECUTABLE]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[BLANK]*/                              
/*[EXECUTABLE]*/                         RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                                              (IntPol7072.CompanyCode                  
/*[EXECUTABLE]*/                                              ,IntPol7072.PolicyNumber                 
/*[EXECUTABLE]*/                                              ,IntPol7072.Rencnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.Endcnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.DocumentUID                  
/*[EXECUTABLE]*/                                              ,IntPol7072.RqUID                                
/*[EXECUTABLE]*/                                              ,""                                       
/*[EXECUTABLE]*/                                              ,""                                       
/*[COMMENT]*/                                                 /* , nv_PrmPrg        /*Name Report*/*/                
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIPolicyNumber               
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIDocumentUID                
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
/*[EXECUTABLE]*/                                              ,OUTPUT nv_SAVEmsgerror).
/*[BLANK]*/                              
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE DO:   /*  2.2  3.2 */
/*[EXECUTABLE]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx\Wctxr712A4_3P3".
/*[EXECUTABLE]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         RUN VALUE(nv_PrgName)
/*[EXECUTABLE]*/                                              (IntPol7072.CompanyCode                  
/*[EXECUTABLE]*/                                              ,IntPol7072.PolicyNumber                 
/*[EXECUTABLE]*/                                              ,IntPol7072.Rencnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.Endcnt /*INTEGER*/           
/*[EXECUTABLE]*/                                              ,IntPol7072.DocumentUID                  
/*[EXECUTABLE]*/                                              ,IntPol7072.RqUID                                
/*[EXECUTABLE]*/                                              ,""                                       
/*[EXECUTABLE]*/                                              ,""                                       
/*[COMMENT]*/                                                 /* , nv_PrmPrg        /*Name Report*/*/                
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIPolicyNumber               
/*[EXECUTABLE]*/                                              ,IntPol7072.CMIDocumentUID                
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[EXECUTABLE]*/                                              ,"0" /*CHARACTER*/                  
/*[COMMENT]*/                                                  /**/
/*[EXECUTABLE]*/                                              ,OUTPUT nv_SAVEmsgerror).   
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.  /* by Kridtiya i. 20/09/2016 */
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[BLANK]*/                      
/*[COMMENT]*/                      /* ---------------------------------------------------- */
/*[EXECUTABLE]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[EXECUTABLE]*/                   NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                   nv_firstchk   = YES.
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
/*[EXECUTABLE]*/                   RUN PD_SAVEPD1ChkFile
/*[EXECUTABLE]*/                       (INPUT nv_NameCompCd
/*[EXECUTABLE]*/                       ,INPUT "V70"
/*[EXECUTABLE]*/                       ,INPUT (IntPol7072.PolicyTypeCd + "T")
/*[EXECUTABLE]*/                       ,OUTPUT nv_SAVEmsgerror).
/*[EXECUTABLE]*/                   IF nv_SAVEmsgerror <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[EXECUTABLE]*/                               + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[EXECUTABLE]*/                               + ".TXT".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     OUTPUT TO VALUE(nv_verror).
/*[EXECUTABLE]*/                     PUT 
/*[EXECUTABLE]*/                     "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                     " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[EXECUTABLE]*/                     " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                     SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     RETURN. 
/*[EXECUTABLE]*/                   END.
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
/*[EXECUTABLE]*/                   IF IntPol7072.CompanyCode  <> "476" THEN nv_covcodtyp1 = (IntPol7072.PolicyTypeCd + "T") . /*Add by kridtiya i. date. 20160121*/
/*[EXECUTABLE]*/                   FOR EACH FNameAttach WHERE
/*[EXECUTABLE]*/                            FNameAttach.CompanyCode  = nv_NameCompCd
/*[EXECUTABLE]*/                        AND FNameAttach.PolicyTypeCd = "V70"
/*[EXECUTABLE]*/                        AND FNameAttach.CoverTypeCd  = nv_covcodtyp1    /*(IntPol7072.PolicyTypeCd + "T")*//*Add by kridtiya i. date. 20160121*/
/*[EXECUTABLE]*/                        AND FNameAttach.EffDate     <= TODAY
/*[EXECUTABLE]*/                   NO-LOCK
/*[EXECUTABLE]*/                   BREAK BY FNameAttach.SelectNumber:
/*[EXECUTABLE]*/                     IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[EXECUTABLE]*/                                                      ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IF FNameAttach.ToFileName   = "" THEN
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[EXECUTABLE]*/                     ELSE DO:
/*[EXECUTABLE]*/                       IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[BLANK]*/                      
/*[EXECUTABLE]*/                            nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[EXECUTABLE]*/                                          + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[EXECUTABLE]*/                       ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     nv_errortext = "".
/*[EXECUTABLE]*/                     IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     loop1:
/*[EXECUTABLE]*/                     REPEAT:
/*[EXECUTABLE]*/                       IF nv_firstchk = NO THEN DO:   
/*[EXECUTABLE]*/                         NV_Lcount = 0.
/*[EXECUTABLE]*/                         DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[EXECUTABLE]*/                           NV_Lcount = NV_Lcount + 1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       nv_firstchk = NO.
/*[EXECUTABLE]*/                       IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[EXECUTABLE]*/                         NV_StartCount = NV_StartCount + NV_Lcount.
/*[EXECUTABLE]*/                         IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[EXECUTABLE]*/                         NEXT loop1.
/*[EXECUTABLE]*/                       END.
/*[COMMENT]*/                          /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[COMMENT]*/                          nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[EXECUTABLE]*/                       DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                          /*
/*[COMMENT]*/                          DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[EXECUTABLE]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[COMMENT]*/                            /*
/*[COMMENT]*/                            NV_Lcount = 0.
/*[COMMENT]*/                            DO  WHILE NV_Lcount <= 1000000:
/*[COMMENT]*/                              NV_Lcount = NV_Lcount + 1.
/*[COMMENT]*/                            END. */
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[EXECUTABLE]*/                         FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[EXECUTABLE]*/                         CREATE TFileAttach.
/*[EXECUTABLE]*/                         TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[COMMENT]*/                            /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[EXECUTABLE]*/                         COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[COMMENT]*/                            /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[COMMENT]*/                            OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[COMMENT]*/                              EXPORT TFileAttach.FileBinary.
/*[COMMENT]*/                            OUTPUT CLOSE. */
/*[EXECUTABLE]*/                         IF ERROR-STATUS:ERROR  THEN DO:
/*[EXECUTABLE]*/                           nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[EXECUTABLE]*/                                        + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[EXECUTABLE]*/                         END.
/*[COMMENT]*/                            /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[COMMENT]*/                            IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[EXECUTABLE]*/                         IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[COMMENT]*/                            /**/
/*[EXECUTABLE]*/                         FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[EXECUTABLE]*/                         NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                         IF AVAILABLE IntS7072 THEN DO:
/*[EXECUTABLE]*/                           nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 1 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN
/*[EXECUTABLE]*/                             IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_LineSeqno = 2 THEN DO:
/*[EXECUTABLE]*/                             ASSIGN
/*[EXECUTABLE]*/                             IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[EXECUTABLE]*/                             IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[EXECUTABLE]*/                             IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           IF nv_errortext <> "" THEN
/*[EXECUTABLE]*/                             ASSIGN
/*[EXECUTABLE]*/                             IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[EXECUTABLE]*/                             IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                       END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[EXECUTABLE]*/                       LEAVE loop1.
/*[EXECUTABLE]*/                     END. /*loop1:*/
/*[EXECUTABLE]*/                   END. /*FOR EACH FNameAttach*/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1FileAtt_Old C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD1FileAtt_Old :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_RECIDIntPol7072 AS RECID NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_RECIDIntS7072   AS RECID NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_errortext     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_INPUTFileName AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_COPYTOFILE    AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_LineSeqno     AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_Lwaitcount    AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_LcountAgain   AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_Lcount        AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_StartCount    AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE NV_LastCount     AS INTEGER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_SAVECompanyNo AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_SAVEmsgerror  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_BrokerCompany AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_BrokerBranch  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Acno1      AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Agent      AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_errort     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_NameCompCd AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_PrgName    AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_PrmPrg     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_firstchk   AS LOGICAL   NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_verror     AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   ASSIGN
/*[UNCALLED]*/                   NV_Lwaitcount = 110000
/*[UNCALLED]*/                   NV_StartCount = 0
/*[UNCALLED]*/                   NV_LastCount  = 6220000. /*3ÇÔ¹Ò·Õ¡ÇèÒæ*/
/*[UNCALLED]*/                   FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[UNCALLED]*/                   nv_LineSeqno = 0.
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                   PUT "1. Print :"
/*[UNCALLED]*/                      /*12345678901*/
/*[UNCALLED]*/                     IntPol7072.CompanyCode 
/*[UNCALLED]*/                     IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                     IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                     TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                     STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                     SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                   SKIP.
/*[UNCALLED]*/                   OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                   /*¾ÔÁ¾ì file pdf Êè§ÍÍ¡*/
/*[UNCALLED]*/                      /* 2.1                             110 */
/*[UNCALLED]*/                   IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> "" THEN DO:
/*[UNCALLED]*/                     RUN WSP/WSPMCpny.P
/*[UNCALLED]*/                          (IntPol7072.CompanyCode
/*[UNCALLED]*/                          ,IntPol7072.BranchCd
/*[UNCALLED]*/                          ,"V70"
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerBranch 
/*[UNCALLED]*/                          ,OUTPUT nv_Acno1
/*[UNCALLED]*/                          ,OUTPUT nv_Agent
/*[UNCALLED]*/                          ,OUTPUT nv_errort).
/*[UNCALLED]*/                     IF nv_errort <> "" THEN RETURN.
/*[UNCALLED]*/                     /* ProgramPrint form PDF ¡ÃÁ¸ÃÃÁì V70 àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                     ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[UNCALLED]*/                     RUN PD_FNameAttach
/*[UNCALLED]*/                         (INPUT IntPol7072.CompanyCode
/*[UNCALLED]*/                         ,INPUT "V70"      /*v70,v72*/
/*[UNCALLED]*/                         ,INPUT IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                         /**/
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrmPrg ).
/*[UNCALLED]*/                     /**/
/*[UNCALLED]*/                     IF    IntPol7072.PolicyTypeCd = "1"
/*[UNCALLED]*/                        OR IntPol7072.PolicyTypeCd = "2"
/*[UNCALLED]*/                        OR IntPol7072.PolicyTypeCd = "3"
/*[UNCALLED]*/                     THEN DO:
/*[UNCALLED]*/                       /* Add by Kridtiya i...Lockton */
/*[UNCALLED]*/                       IF IntPol7072.CompanyCode = "469" THEN DO:   
/*[UNCALLED]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr708".
/*[UNCALLED]*/                         ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /*Wctxr708.p Form 1 Policy*/
/*[UNCALLED]*/                         RUN VALUE(nv_PrgName)        
/*[UNCALLED]*/                             (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[UNCALLED]*/                             ,IntPol7072.PolicyNumber
/*[UNCALLED]*/                             ,IntPol7072.Rencnt
/*[UNCALLED]*/                             ,IntPol7072.Endcnt
/*[UNCALLED]*/                             ,IntPol7072.DocumentUID
/*[UNCALLED]*/                             ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                             ,""               /*n_user   */
/*[UNCALLED]*/                             ,""               /*n_passwd */
/*[UNCALLED]*/                             ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                         RUN proc_FileAttach2               /* print cer */
/*[UNCALLED]*/                             (INPUT IntPol7072.CompanyCode
/*[UNCALLED]*/                             ,INPUT "V70"      /*v70,v72*/
/*[UNCALLED]*/                             ,INPUT IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                             /**/
/*[UNCALLED]*/                             ,INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                             ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                             ,INPUT-OUTPUT nv_PrmPrg ).
/*[UNCALLED]*/                       END. /*End add Kridtiya i. */
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         /*IF  IntPol7072.PolicyTypeCd = "3"    OR*/ /*kridtiya i.*/
/*[UNCALLED]*/                          IF  (IntPol7072.PolicyTypeCd = "3"   AND substr(IntPol7072.PolicyNumber,1,1) <> "R" ) OR  /*kridtiya i.*/
/*[UNCALLED]*/                             /*TEST Prn cover 1*/
/*[UNCALLED]*/                            (IntPol7072.CompanyCode  = "242" AND IntPol7072.PolicyTypeCd = "1" )
/*[UNCALLED]*/                             /*Isuzu */
/*[UNCALLED]*/                         OR (IntPol7072.CompanyCode  = "476" AND IntPol7072.PolicyTypeCd = "1" )
/*[UNCALLED]*/                         THEN DO:
/*[UNCALLED]*/                           IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr701_1".
/*[UNCALLED]*/                                              ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr701A4.p*/
/*[UNCALLED]*/                           RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                              (IntPol7072.CompanyCode  /*nv_BrokerCompany*/
/*[UNCALLED]*/                              ,IntPol7072.PolicyNumber
/*[UNCALLED]*/                              ,IntPol7072.Rencnt
/*[UNCALLED]*/                              ,IntPol7072.Endcnt
/*[UNCALLED]*/                              ,IntPol7072.DocumentUID
/*[UNCALLED]*/                              ,IntPol7072.RqUID /*nv_code keyRequestIndRq*/
/*[UNCALLED]*/                              ,""               /*n_user  */
/*[UNCALLED]*/                              ,""               /*n_passwd */
/*[UNCALLED]*/                              ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       /*».3.1 + àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                       /*ãºàÊÃç¨/ãº¡Ó¡ÑºÀÒÉÕ à©¾ÒÐ »3, 3+*/
/*[UNCALLED]*/                       IF SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[UNCALLED]*/                          OR  ((IntPol7072.CompanyCode = "833" OR IntPol7072.CompanyCode = "834" 
/*[UNCALLED]*/                          OR    IntPol7072.CompanyCode = "442" OR IntPol7072.CompanyCode = "701"
/*[UNCALLED]*/                          OR    IntPol7072.CompanyCode = "242" OR IntPol7072.CompanyCode = "476" ) /*isuzu*/
/*[UNCALLED]*/                              /**/
/*[UNCALLED]*/                          AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2") )
/*[UNCALLED]*/                               /*·Ø»»ÃÐ¡Ñ¹ 1 áÊ¹¾ÔÁ¾ì¡ÃÁ¸ÃÃÁìä´é*/
/*[UNCALLED]*/                       THEN DO:
/*[UNCALLED]*/                         IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr703_1".
/*[UNCALLED]*/                                            ELSE nv_PrgName = "Wctx/" + nv_PrgName. /*Wctxr703A4*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                         RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                             (IntPol7072.CompanyCode
/*[UNCALLED]*/                             ,IntPol7072.PolicyNumber
/*[UNCALLED]*/                             ,IntPol7072.Rencnt
/*[UNCALLED]*/                             ,IntPol7072.Endcnt
/*[UNCALLED]*/                             ,IntPol7072.DocumentUID 
/*[UNCALLED]*/                             ,IntPol7072.RqUID
/*[UNCALLED]*/                             ,""
/*[UNCALLED]*/                             ,""
/*[UNCALLED]*/                             ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     /* ---------------------------------------------------- */
/*[UNCALLED]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072
/*[UNCALLED]*/                     NO-ERROR NO-WAIT.
/*[UNCALLED]*/                     nv_firstchk   = YES.
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "2. ChkPDF:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     PAUSE 1 NO-MESSAGE.
/*[UNCALLED]*/                     RUN PD_SAVEPD1ChkFile
/*[UNCALLED]*/                         (INPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT "V70"
/*[UNCALLED]*/                         ,INPUT IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN DO:
/*[UNCALLED]*/                       nv_verror = "ERRPDF70_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[UNCALLED]*/                                 + ".TXT".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       OUTPUT TO VALUE(nv_verror).
/*[UNCALLED]*/                       PUT 
/*[UNCALLED]*/                       "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[UNCALLED]*/                       " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[UNCALLED]*/                       " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                       SKIP.
/*[UNCALLED]*/                       OUTPUT CLOSE.
/*[UNCALLED]*/                       RETURN. 
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     /* ËéÒÁ Åº µéÍ§ÃÍ PDFCreator file pdf ÁÔ©Ð¹Ñé¹¨ÐÁÕáµèª×èÍ File */
/*[UNCALLED]*/                     /* -------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "3. SvFile:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     FOR EACH FNameAttach WHERE
/*[UNCALLED]*/                              FNameAttach.CompanyCode  = nv_NameCompCd
/*[UNCALLED]*/                          AND FNameAttach.PolicyTypeCd = "V70"
/*[UNCALLED]*/                          AND FNameAttach.CoverTypeCd  = IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                          AND FNameAttach.EffDate     <= TODAY
/*[UNCALLED]*/                     NO-LOCK
/*[UNCALLED]*/                     BREAK BY FNameAttach.SelectNumber:
/*[UNCALLED]*/                       IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[UNCALLED]*/                                                        ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       IF FNameAttach.ToFileName   = "" THEN
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber) + ".PDF".
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[UNCALLED]*/                   
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.PolicyNumber)          /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[UNCALLED]*/                                            + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[UNCALLED]*/                         ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       nv_errortext = "".
/*[UNCALLED]*/                       IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       loop1:
/*[UNCALLED]*/                       REPEAT:
/*[UNCALLED]*/                         IF nv_firstchk = NO THEN DO:   
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         nv_firstchk = NO.
/*[UNCALLED]*/                         IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[UNCALLED]*/                           NV_StartCount = NV_StartCount + NV_Lcount.
/*[UNCALLED]*/                           IF NV_StartCount >= NV_LastCount THEN LEAVE loop1.
/*[UNCALLED]*/                           NEXT loop1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         /* nv_INPUTFileName = "FormCMI.PDF". /*"D:\WebBU\FormCMI.PDF".*/
/*[UNCALLED]*/                         nv_COPYTOFILE    = TRIM(IntPol7072.PolicyNumber) + ".PDF". */
/*[UNCALLED]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                         /*
/*[UNCALLED]*/                         DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) = ? THEN DO:
/*[UNCALLED]*/                           DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                           /*
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= 1000000:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END. */
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[UNCALLED]*/                           FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                           CREATE TFileAttach.
/*[UNCALLED]*/                           TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[UNCALLED]*/                           /* TFileAttach.FileNameAttach = TRIM(IntPol7072.PolicyNumber) + ".PDF".*/
/*[UNCALLED]*/                           COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[UNCALLED]*/                           /* INPUTFileName  = "D:\TEMP\DBBUInt.zip".
/*[UNCALLED]*/                           OUTPUT TO FileAtt1.pdf BINARY NO-CONVERT.
/*[UNCALLED]*/                             EXPORT TFileAttach.FileBinary.
/*[UNCALLED]*/                           OUTPUT CLOSE. */
/*[UNCALLED]*/                           IF ERROR-STATUS:ERROR  THEN DO:
/*[UNCALLED]*/                             nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[UNCALLED]*/                                          + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           /* äÁèdelete à¾×èÍ monitor / ãªé¨ÃÔ§ àÍÒ remark ÍÍ¡
/*[UNCALLED]*/                           IF SEARCH(nv_COPYTOFILE) <> ? THEN DOS SILENT DEL VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                           IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[UNCALLED]*/                           /**/
/*[UNCALLED]*/                           FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[UNCALLED]*/                           NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE IntS7072 THEN DO:
/*[UNCALLED]*/                             nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data*/
/*[UNCALLED]*/                             IF nv_LineSeqno = 1 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_LineSeqno = 2 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_errortext <> "" THEN
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[UNCALLED]*/                               IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END. /*IF SEARCH(nv_COPYTOFILE) <> ?*/
/*[UNCALLED]*/                         LEAVE loop1.
/*[UNCALLED]*/                       END. /*loop1:*/
/*[UNCALLED]*/                     END. /*FOR EACH FNameAttach*/
/*[UNCALLED]*/                   END. /*IF IntPol7072.PolicyTypeCd <> "" AND IntPol7072.RateGroup <> ""*/
/*[UNCALLED]*/                   /*  -------------------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /* ¾Ãº. / Compulsory */
/*[UNCALLED]*/                   IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" THEN DO:
/*[UNCALLED]*/                     RUN WSP/WSPMCpny.P
/*[UNCALLED]*/                          (IntPol7072.CompanyCode
/*[UNCALLED]*/                          ,IntPol7072.BranchCd
/*[UNCALLED]*/                          ,"V72"
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerCompany /*ËÒºÃÔÉÑ··Õèsetã¹ÃÐºº*/
/*[UNCALLED]*/                          ,OUTPUT nv_BrokerBranch 
/*[UNCALLED]*/                          ,OUTPUT nv_Acno1
/*[UNCALLED]*/                          ,OUTPUT nv_Agent
/*[UNCALLED]*/                          ,OUTPUT nv_errort).
/*[UNCALLED]*/                     IF nv_errort <> "" THEN RETURN.
/*[UNCALLED]*/                     NV_StartCount = 0.
/*[UNCALLED]*/                     /* ---------------------------------------------------- */
/*[UNCALLED]*/                     /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                     ASSIGN nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     RUN PD_FNameAttach
/*[UNCALLED]*/                         (INPUT        IntPol7072.CompanyCode
/*[UNCALLED]*/                         ,INPUT        "V72"      /*v70,v72*/
/*[UNCALLED]*/                         ,INPUT        IntPol7072.CMIPolicyTypeCd
/*[UNCALLED]*/                         /**/
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                         ,INPUT-OUTPUT nv_PrmPrg ).
/*[UNCALLED]*/                     /**/
/*[UNCALLED]*/                     /*Blank form Compulsory */
/*[UNCALLED]*/                     IF IntPol7072.CompanyCode = "833" THEN DO:
/*[UNCALLED]*/                       RUN PD_ChkBlankForm
/*[UNCALLED]*/                            (INPUT-OUTPUT nv_NameCompCd
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_PrgName
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_PrmPrg).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_PrgName = "" THEN DO:
/*[UNCALLED]*/                       RUN Wctx/wctxr702.P
/*[UNCALLED]*/                            (IntPol7072.CompanyCode     /*nv_BrokerCompany*/
/*[UNCALLED]*/                            ,IntPol7072.CMIPolicyNumber
/*[UNCALLED]*/                            ,IntPol7072.Rencnt
/*[UNCALLED]*/                            ,IntPol7072.Endcnt
/*[UNCALLED]*/                            ,IntPol7072.CMIDocumentUID
/*[UNCALLED]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                            ,""         /*n_user   */
/*[UNCALLED]*/                            ,""         /*n_passwd */
/*[UNCALLED]*/                            ,nv_PrmPrg  /*Name Report*/
/*[UNCALLED]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       nv_PrgName = "Wctx/" + nv_PrgName.
/*[UNCALLED]*/                       /*
/*[UNCALLED]*/                       RUN Wctx/wctxr702A4.P ( IntPol7072.CompanyCode */
/*[UNCALLED]*/                       RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                            (IntPol7072.CompanyCode   /*nv_BrokerCompany*/
/*[UNCALLED]*/                            ,IntPol7072.CMIPolicyNumber
/*[UNCALLED]*/                            ,IntPol7072.Rencnt
/*[UNCALLED]*/                            ,IntPol7072.Endcnt
/*[UNCALLED]*/                            ,IntPol7072.CMIDocumentUID
/*[UNCALLED]*/                            ,IntPol7072.RqUID /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                            ,""        /*n_user   */
/*[UNCALLED]*/                            ,""        /*n_passwd */
/*[UNCALLED]*/                            ,nv_PrmPrg /*Name Report="V72A4"*/
/*[UNCALLED]*/                            ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN RETURN.
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "2. ChkPDF:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     PAUSE 1 NO-MESSAGE.
/*[UNCALLED]*/                     RUN PD_SAVEPD1ChkFile
/*[UNCALLED]*/                         (INPUT nv_NameCompCd
/*[UNCALLED]*/                         ,INPUT "V72"
/*[UNCALLED]*/                         ,INPUT IntPol7072.CMIPolicyTypeCd
/*[UNCALLED]*/                         ,OUTPUT nv_SAVEmsgerror).
/*[UNCALLED]*/                     IF nv_SAVEmsgerror <> "" THEN DO:
/*[UNCALLED]*/                       nv_verror = "ERRPDF72_" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
/*[UNCALLED]*/                                 + SUBSTR(STRING(TIME,"HH:MM:SS"),7,2)
/*[UNCALLED]*/                                 + ".TXT".
/*[UNCALLED]*/                       OUTPUT TO VALUE(nv_verror).
/*[UNCALLED]*/                       PUT 
/*[UNCALLED]*/                       "Not found file pdf: Company: " IntPol7072.CompanyCode 
/*[UNCALLED]*/                       " Policy no.: "   IntPol7072.PolicyNumber   FORMAT "X(16)"
/*[UNCALLED]*/                       " Contract no.: " IntPol7072.ContractNumber FORMAT "X(20)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                       SKIP.
/*[UNCALLED]*/                       OUTPUT CLOSE.
/*[UNCALLED]*/                       RETURN. 
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     /* -------------------------------------------------------------------------------- */
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                     OUTPUT TO PrnTIME.txt APPEND.
/*[UNCALLED]*/                     PUT "3. SvFile:"
/*[UNCALLED]*/                        /*1234567890*/
/*[UNCALLED]*/                       IntPol7072.CompanyCode 
/*[UNCALLED]*/                       IntPol7072.PolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       IntPol7072.CMIPolicyNumber FORMAT "X(16)"
/*[UNCALLED]*/                       TODAY FORMAT "99/99/9999" " "
/*[UNCALLED]*/                       STRING(TIME,"HH:MM:SS")   "." 
/*[UNCALLED]*/                       SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     ASSIGN
/*[UNCALLED]*/                     nv_INPUTFileName = "" nv_COPYTOFILE = "" nv_firstchk = YES.
/*[UNCALLED]*/                     FOR EACH FNameAttach WHERE
/*[UNCALLED]*/                              FNameAttach.CompanyCode  = nv_NameCompCd
/*[UNCALLED]*/                          AND FNameAttach.PolicyTypeCd = "V72"
/*[UNCALLED]*/                          AND FNameAttach.CoverTypeCd  = IntPol7072.CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T"*/
/*[UNCALLED]*/                          AND FNameAttach.EffDate     <= TODAY
/*[UNCALLED]*/                     NO-LOCK
/*[UNCALLED]*/                     BREAK BY FNameAttach.SelectNumber
/*[UNCALLED]*/                     :
/*[UNCALLED]*/                       IF FNameAttach.CopyFileName = "" THEN LEAVE.
/*[UNCALLED]*/                                                        ELSE nv_INPUTFileName = FNameAttach.CopyFileName.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       IF FNameAttach.ToFileName   = "" THEN
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         IF INDEX(FNameAttach.ToFileName, ".PDF") = 0 THEN
/*[UNCALLED]*/                              nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber)       /*àºÍÃì¡ÃÁ¸ÃÃÁì*/
/*[UNCALLED]*/                                            + TRIM(FNameAttach.ToFileName) + ".PDF". /*µÑÇÂèÍ§Ò¹*/
/*[UNCALLED]*/                         ELSE nv_COPYTOFILE = FNameAttach.ToFileName.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       nv_errortext = "".
/*[UNCALLED]*/                       IF nv_INPUTFileName = "" OR nv_COPYTOFILE = "" THEN LEAVE.
/*[UNCALLED]*/                       IF TRIM(nv_COPYTOFILE) = ".PDF" THEN
/*[UNCALLED]*/                               nv_COPYTOFILE = TRIM(IntPol7072.CMIPolicyNumber) + ".PDF".
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       loop2:
/*[UNCALLED]*/                       REPEAT:
/*[UNCALLED]*/                         IF nv_firstchk = NO THEN DO:   
/*[UNCALLED]*/                           NV_Lcount = 0.
/*[UNCALLED]*/                           DO  WHILE NV_Lcount <= NV_Lwaitcount:
/*[UNCALLED]*/                             NV_Lcount = NV_Lcount + 1.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         nv_firstchk = NO.
/*[UNCALLED]*/                         IF SEARCH(nv_INPUTFileName) = ? THEN DO:
/*[UNCALLED]*/                           NV_StartCount = NV_StartCount + NV_Lcount.
/*[UNCALLED]*/                           IF NV_StartCount >= NV_LastCount THEN LEAVE loop2.
/*[UNCALLED]*/                           NEXT loop2.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         DOS SILENT COPY VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE)  /Y.
/*[UNCALLED]*/                         /*
/*[UNCALLED]*/                         DOS SILENT RENAME VALUE(nv_INPUTFileName) VALUE(nv_COPYTOFILE). */
/*[UNCALLED]*/                         IF SEARCH(nv_COPYTOFILE) <> ? THEN DO:
/*[UNCALLED]*/                           FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                           CREATE TFileAttach.
/*[UNCALLED]*/                           TFileAttach.FileNameAttach = nv_COPYTOFILE.
/*[UNCALLED]*/                           COPY-LOB FROM FILE nv_COPYTOFILE TO TFileAttach.FileBinary NO-CONVERT NO-ERROR.
/*[UNCALLED]*/                           IF ERROR-STATUS:ERROR  THEN DO:
/*[UNCALLED]*/                             nv_errortext = "äÁèÊÒÁÒÃ¶ Load File: " + TRIM(nv_COPYTOFILE) + " "
/*[UNCALLED]*/                                          + ERROR-STATUS:GET-MESSAGE(1) + ERROR-STATUS:GET-MESSAGE(2).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           IF SEARCH(nv_INPUTFileName) <> ? THEN DOS SILENT DEL VALUE(nv_INPUTFileName).
/*[UNCALLED]*/                           /**/
/*[UNCALLED]*/                           FIND IntS7072 WHERE RECID(IntS7072) = nv_RECIDIntS7072
/*[UNCALLED]*/                           NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE IntS7072 THEN DO:
/*[UNCALLED]*/                             nv_LineSeqno = nv_LineSeqno + 1. /*ÅÓ´Ñº¡ÒÃ add data ÍÔ§¡Ñº´éÒ¹º¹*/
/*[UNCALLED]*/                             IF nv_LineSeqno = 1 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach1 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile1     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach1   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile1       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_LineSeqno = 2 THEN DO:
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.FileNameAttach2 = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntPol7072.AttachFile2     = TFileAttach.FileBinary
/*[UNCALLED]*/                               IntS7072.FileNameAttach2   = TFileAttach.FileNameAttach
/*[UNCALLED]*/                               IntS7072.AttachFile2       = TFileAttach.FileBinary .
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             IF nv_errortext <> "" THEN
/*[UNCALLED]*/                               ASSIGN
/*[UNCALLED]*/                               IntPol7072.RemarkText = TRIM(TRIM(IntPol7072.RemarkText) + " " + nv_errortext).
/*[UNCALLED]*/                               IntS7072.RemarkText   = TRIM(TRIM(IntS7072.RemarkText)   + " " + nv_errortext).
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         LEAVE loop2.
/*[UNCALLED]*/                       END. /*loop2:*/
/*[UNCALLED]*/                     END. /*FOR EACH FNameAttach*/
/*[UNCALLED]*/                   END. /*IF IntPol7072.CMIPolicyTypeCd <> "" AND IntPol7072.CMIVehTypeCd <> "" */
/*[UNCALLED]*/                   FOR EACH TFileAttach: DELETE TFileAttach. END.
/*[UNCALLED]*/                   RELEASE FNameAttach.  */
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1F_PrgName C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1F_PrgName :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT        PARAMETER nv_RECIDIntPol7072  AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT-OUTPUT PARAMETER nv_PrgName          AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 ASSIGN nv_covcodtyp1 = "".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF AVAIL IntPol7072 THEN DO:
/*[COMMENT]*/                        /* Line : 70 + 72 compulsaly */
/*[EXECUTABLE]*/                     IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:  
/*[EXECUTABLE]*/                         IF IntPol7072.PolicyTypeCd = "1" THEN DO: 
/*[EXECUTABLE]*/                             ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "1T". 
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
/*[EXECUTABLE]*/                         END.                                                         
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3"                        THEN ASSIGN nv_PrgName = "wctxis72A4"   nv_covcodtyp1 = "3T". 
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "2.1"                      THEN ASSIGN nv_PrgName = "wctxis74A4"   nv_covcodtyp1 = "2.1T". 
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3.1"                      THEN ASSIGN nv_PrgName = "wctxis74A4"   nv_covcodtyp1 = "3.1T". 
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "2.2"                      THEN ASSIGN nv_PrgName = "wctxis78A4"   nv_covcodtyp1 = "2.2T". 
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3.2"                      THEN ASSIGN nv_PrgName = "wctxis78A4"   nv_covcodtyp1 = "3.2T". 
/*[BLANK]*/                      
/*[COMMENT]*/                            /*4U ». 2+ 3+   ÍÂèÒ§à´ÕÂÇ  ** cover 2.1  3.1   wctxizu707.p  SCHEDULE8 * +++ *  */
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     ELSE DO:
/*[COMMENT]*/                            /* Line : 70  only */
/*[EXECUTABLE]*/                         IF IntPol7072.PolicyTypeCd = "1" THEN DO: 
/*[EXECUTABLE]*/                             ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "1". 
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
/*[EXECUTABLE]*/                         END.      
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3"                        THEN ASSIGN nv_PrgName = "wctxis71A4"   nv_covcodtyp1 = "3". 
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "2.1"                      THEN ASSIGN nv_PrgName = "wctxis73A4"   nv_covcodtyp1 = "2.1".
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3.1"                      THEN ASSIGN nv_PrgName = "wctxis73A4"   nv_covcodtyp1 = "3.1".
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "2.2"                      THEN ASSIGN nv_PrgName = "wctxis77A4"   nv_covcodtyp1 = "2.2".
/*[EXECUTABLE]*/                         ELSE IF IntPol7072.PolicyTypeCd = "3.2"                      THEN ASSIGN nv_PrgName = "wctxis77A4"   nv_covcodtyp1 = "3.2". 
/*[COMMENT]*/                            /*4U ». 2+ 3+   ÍÂèÒ§à´ÕÂÇ  ** cover 2.1  3.1   wctxizu707.p  SCHEDULE7 * +++ *  */    
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 OUTPUT TO PD_SAVEPD1F_PrgName.TXT APPEND.
/*[EXECUTABLE]*/                 PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                   nv_covcodtyp1 FORMAT "x(5)"
/*[COMMENT]*/                      /*
/*[EXECUTABLE]*/                   nv_NameCompCd FORMAT "x(20)" /*210*/ */
/*[EXECUTABLE]*/                   nv_PrgName    FORMAT "x(20)" /*Wctxr701A4*/
/*[COMMENT]*/                      /*
/*[EXECUTABLE]*/                   nv_PrmPrg     FORMAT "x(20)" /*V70A4*/ */
/*[EXECUTABLE]*/                 SKIP.
/*[EXECUTABLE]*/                 OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1IntS C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1IntS :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       à¡çº¢éÍÁÙÅ ·Ø¹»ÃÐ¡Ñ¹ àºÕéÂÊØ·¸Ô ÍÒ¡Ã áÊµÁ»ì ÊÓËÃÑºÊè§µÍº¡ÅÑº
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_rec_rq          AS RECID NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_RecIntPol7072   AS RECID NO-UNDO.
/*[COMMENT]*/                    /* ------------------------------------------------------ */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                 NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÀÒ¤ÊÁÑ¤Ãã¨ */
/*[EXECUTABLE]*/                 ASSIGN
/*[COMMENT]*/                      /*
/*[EXECUTABLE]*/                   = IntPol7072.DRIVER_TITLE_1    /*uwm100.policy    /Policy no.*/ */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IntS7072.COLLAmtAccident = IntPol7072.DRIVER_NAME_1      /*nv_SUM_INSURE    /COLLAmtAccident*/
/*[EXECUTABLE]*/                   IntS7072.FTAmt           = IntPol7072.DRIVER_SURNAME_1   /*nv_SUM_INSUREAcc /FTAmt*/
/*[EXECUTABLE]*/                   IntS7072.WrittenAmt      = IntPol7072.DRIVER_GENDER_1    /*uwm100.prem_t    /WrittenAmt*/ 
/*[EXECUTABLE]*/                   IntS7072.RevenueStampAmt = IntPol7072.DRIVER_AGE_1       /*uwm100.rstp_t    /RevenueStampAmt*/
/*[EXECUTABLE]*/                   IntS7072.VatAmt          = IntPol7072.DRIVER_LICENSE_1.  /*uwm100.rtax_t    /VatAmt*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.CurrentTermAmt  = STRING( DECIMAL(IntS7072.WrittenAmt)
/*[EXECUTABLE]*/                                                  + DECIMAL(IntS7072.RevenueStampAmt)
/*[EXECUTABLE]*/                                                  + DECIMAL(IntS7072.VatAmt), ">>>,>>>,>>9.99") NO-ERROR.
/*[COMMENT]*/                    /* ------------------- */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ÀÒ¤ºÑ§¤Ñº (¾.Ã.º.) */
/*[EXECUTABLE]*/                 ASSIGN
/*[COMMENT]*/                      /*
/*[EXECUTABLE]*/                   = IntPol7072.DRIVER_TITLE_2    /*uwm100.policy    /Policy no.*/ */
/*[EXECUTABLE]*/                   IntS7072.CMIAmtPerson       = IntPol7072.DRIVER_NMAE_2     /*nv_SUM_INSURE    /COLLAmtAccident*/
/*[EXECUTABLE]*/                   IntS7072.CMIAmtAccident     = IntPol7072.DRIVER_SURNAME_2  /*nv_SUM_INSUREAcc /FTAmt*/
/*[EXECUTABLE]*/                   IntS7072.CMIWrittenAmt      = IntPol7072.DRIVER_GENDER_2   /*uwm100.prem_t    /WrittenAmt*/
/*[EXECUTABLE]*/                   IntS7072.CMIRevenueStampAmt = IntPol7072.DRIVER_AGE_2      /*uwm100.rstp_t    /RevenueStampAmt*/
/*[EXECUTABLE]*/                   IntS7072.CMIVatAmt          = IntPol7072.DRIVER_LICENSE_2. /*uwm100.rtax_t    /VatAmt*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntS7072.CMICurrentTermAmt  = STRING( DECIMAL(IntS7072.CMIWrittenAmt)
/*[EXECUTABLE]*/                                                     + DECIMAL(IntS7072.CMIRevenueStampAmt)
/*[EXECUTABLE]*/                                                     + DECIMAL(IntS7072.CMIVatAmt), ">>>,>>>,>>9.99") NO-ERROR.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* ------------------- */
/*[EXECUTABLE]*/                 OUTPUT STREAM xmlstream TO PUT_PD_SAVEPD1IntS.TXT.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "                   |" TODAY FORMAT "99/99/9999" 
/*[EXECUTABLE]*/                     " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3) SKIP.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "27 WrittenAmt      |" IntPol7072.DRIVER_NMAE_2      SKIP.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "5  CMIPolicyNumber |" IntPol7072.DRIVER_SURNAME_2   FORMAT "x(20)" SKIP.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "7  CMIDocumentUID  |" IntPol7072.DRIVER_GENDER_2    FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "7  CMIBarCodeNumber|" IntPol7072.DRIVER_AGE_2       FORMAT "X(20)" SKIP.
/*[EXECUTABLE]*/                 PUT STREAM xmlstream "32 CMIPolicyTypeCd |" IntPol7072.DRIVER_LICENSE_2   SKIP. 
/*[EXECUTABLE]*/                 PUT STREAM xmlstream  FILL("-",78) FORMAT "X(78)" SKIP(1).
/*[EXECUTABLE]*/                 OUTPUT STREAM xmlstream CLOSE.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_01 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1_01 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_okprn   AS LOGICAL   NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_trty11  AS CHARACTER NO-UNDO.   /*M,T*/
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_docno1  AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_STdocno AS CHARACTER NO-UNDO.
/*[EXECUTABLE]*/                 DEFINE INPUT PARAMETER nv_STKNo   AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    /*DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.*/
/*[COMMENT]*/                    /*DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
/*[COMMENT]*/                    DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF  /* SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3" OR                IntPol7072.CMIPolicyTypeCd    <> ""*/
/*[COMMENT]*/                     /*  OR ((IntPol7072.CompanyCode = "833") OR (IntPol7072.CompanyCode = "570" ) OR (IntPol7072.CompanyCode = "839") OR (IntPol7072.CompanyCode = "1098")
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "210")  OR (IntPol7072.CompanyCode = "M82")  OR (IntPol7072.CompanyCode = "444") OR (IntPol7072.CompanyCode = "1103") 
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "1141") OR (IntPol7072.CompanyCode = "1107") OR (IntPol7072.CompanyCode = "1056") OR (IntPol7072.CompanyCode = "M73") 
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "1012") OR (IntPol7072.CompanyCode = "M85")  OR (IntPol7072.CompanyCode = "1146") OR (IntPol7072.CompanyCode = "1018")
/*[COMMENT]*/                       OR (IntPol7072.CompanyCode = "834")  OR (IntPol7072.CompanyCode = "1470") OR (IntPol7072.CompanyCode = "1554") OR (IntPol7072.CompanyCode = "1752")
/*[COMMENT]*/                       AND */
/*[EXECUTABLE]*/                        (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q"  /* AND nv_okprn = YES*/   )
/*[COMMENT]*/                    /**AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2")
/*[COMMENT]*/                       AND DECIMAL(IntPol7072.SumInsureAmt) <= 200000) */
/*[EXECUTABLE]*/                 THEN DO:
/*[EXECUTABLE]*/                   IF     IntPol7072.DocumentUID      = "" /*nv_DocnoV70*/
/*[EXECUTABLE]*/                      AND IntPol7072.CMIDocumentUID   = "" /*nv_DocnoV72*/
/*[COMMENT]*/                       /*  AND IntS7072.StickerNumber    = ""
/*[COMMENT]*/                         AND IntS7072.CMIBarCodeNumber = "" */
/*[EXECUTABLE]*/                   THEN DO:  
/*[EXECUTABLE]*/                     ASSIGN nv_trty11 = "" nv_docno1 = "" nv_STdocno = "".
/*[EXECUTABLE]*/                     IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3" OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "2" /*
/*[COMMENT]*/                            ((IntPol7072.CompanyCode = "833")  OR (IntPol7072.CompanyCode = "834")  OR (IntPol7072.CompanyCode = "570")  OR (IntPol7072.CompanyCode = "210")
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1098") OR (IntPol7072.CompanyCode = "M85")  OR (IntPol7072.CompanyCode = "M82")  OR (IntPol7072.CompanyCode = "M73") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1103") OR (IntPol7072.CompanyCode = "1107") OR (IntPol7072.CompanyCode = "1141") OR (IntPol7072.CompanyCode = "1146") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "1018") OR (IntPol7072.CompanyCode = "444")  OR (IntPol7072.CompanyCode = "1056") OR (IntPol7072.CompanyCode = "1012") 
/*[COMMENT]*/                          OR (IntPol7072.CompanyCode = "839")  OR (IntPol7072.CompanyCode = "1470") OR (IntPol7072.CompanyCode = "1554") OR (IntPol7072.CompanyCode = "1752"))
/*[COMMENT]*/                          AND ( IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2" OR  IntPol7072.PolicyTypeCd = "2.3" 
/*[EXECUTABLE]*/                          OR IntPol7072.PolicyTypeCd = "2.4" OR IntPol7072.PolicyTypeCd = "2.5" OR IntPol7072.PolicyTypeCd = "2.6" )*/ THEN DO:
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
/*[EXECUTABLE]*/                         nv_STdocno = "YES".
/*[EXECUTABLE]*/                         IF nv_STdocno = "YES" THEN DO:
/*[EXECUTABLE]*/                             nv_trty11       = "M".
/*[EXECUTABLE]*/                             nv_msgerror7072 = "".
/*[COMMENT]*/                                /*RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072).*/
/*[COMMENT]*/                                /*RUN WXZ\WXZCDOC0.p (INPUT  1    /*nv_choice */*/
/*[EXECUTABLE]*/                             RUN WXZ\WXZCDOCWOS.p (INPUT  1    /*nv_choice */
/*[EXECUTABLE]*/                                            ,INPUT  "M"       /*nv_DocType*/
/*[EXECUTABLE]*/                                            ,INPUT  IntPol7072.CompanyCode    
/*[EXECUTABLE]*/                                            ,INPUT  IntPol7072.Username      
/*[EXECUTABLE]*/                                            ,INPUT  NO            
/*[EXECUTABLE]*/                                            ,OUTPUT nv_docno1
/*[EXECUTABLE]*/                                            ,OUTPUT nv_docrun2
/*[EXECUTABLE]*/                                            ,OUTPUT nv_docrun3
/*[EXECUTABLE]*/                                            ,OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                             ASSIGN
/*[EXECUTABLE]*/                                 IntPol7072.DocumentUID = nv_docno1
/*[EXECUTABLE]*/                                 IntS7072.DocumentUID   = nv_docno1.
/*[EXECUTABLE]*/                         END.
/*[EXECUTABLE]*/                         OUTPUT TO PD_SAVEPD_01pin_BLKDOC.TXT APPEND.
/*[EXECUTABLE]*/                         PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                             " PD_SAVEPD1_01: 70. Start: " FORMAT "x(25)"   SKIP.
/*[EXECUTABLE]*/                         PUT "company : " IntPol7072.CompanyCode FORMAT "x(5)"   
/*[EXECUTABLE]*/                             " nv_STdocno: " nv_STdocno FORMAT "x(5)"  
/*[EXECUTABLE]*/                           " nv_docno1: "  nv_docno1  FORMAT "9999999"
/*[EXECUTABLE]*/                            " nv_STKNo:  "  nv_STKNo  FORMAT "x(15)"  
/*[EXECUTABLE]*/                         SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                         ASSIGN
/*[EXECUTABLE]*/                             IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                       IntPol7072.GenSicBran     = NO
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranText = nv_msgerror7072
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranST   = "ERROR"
/*[EXECUTABLE]*/                       IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[EXECUTABLE]*/                       IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                          IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[EXECUTABLE]*/                       OUTPUT TO PD_SAVEPD_01pin_BLKDOC.TXT APPEND.
/*[EXECUTABLE]*/                       PUT "70 : ERROR" SKIP.
/*[EXECUTABLE]*/                       OUTPUT CLOSE.
/*[EXECUTABLE]*/                       RETURN.
/*[EXECUTABLE]*/                     END.
/*[COMMENT]*/                        /**/
/*[EXECUTABLE]*/                     IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
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
/*[EXECUTABLE]*/                       nv_STdocno = "YES".
/*[EXECUTABLE]*/                       IF nv_STdocno = "YES" THEN DO:
/*[EXECUTABLE]*/                         nv_trty11       = "T".
/*[EXECUTABLE]*/                         nv_msgerror7072 = "".
/*[COMMENT]*/                            /*RUN WSP/WSPFRecp.p
/*[COMMENT]*/                                (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[COMMENT]*/                                ,nv_trty11
/*[COMMENT]*/                                ,OUTPUT nv_docno1
/*[COMMENT]*/                                ,OUTPUT nv_STKNo
/*[COMMENT]*/                                ,OUTPUT nv_msgerror7072).*/
/*[COMMENT]*/                            /*RUN WXZ\WXZCDOC0.p (INPUT  1        /*nv_choice */*/
/*[EXECUTABLE]*/                         RUN WXZ\WXZCDOCWOS.p (INPUT  1        /*nv_choice */
/*[EXECUTABLE]*/                                             ,INPUT  "M"       /*nv_DocType*/
/*[EXECUTABLE]*/                                             ,INPUT  IntPol7072.CompanyCode  
/*[EXECUTABLE]*/                                             ,INPUT  IntPol7072.Username      
/*[EXECUTABLE]*/                                             ,INPUT  NO  
/*[EXECUTABLE]*/                                             ,OUTPUT nv_docno1
/*[EXECUTABLE]*/                                             ,OUTPUT nv_docrun2
/*[EXECUTABLE]*/                                             ,OUTPUT nv_docrun3
/*[EXECUTABLE]*/                                             ,OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                         IF nv_msgerror7072 = "" THEN
/*[COMMENT]*/                                /*RUN WXZ\WXZCDOC0.p (INPUT  1        /*nv_choice */*/
/*[EXECUTABLE]*/                             RUN WXZ\WXZCDOCWOS.p (INPUT  1        /*nv_choice */
/*[EXECUTABLE]*/                                                 ,INPUT  "T"       /*nv_DocType*/
/*[EXECUTABLE]*/                                                 ,INPUT  IntPol7072.CompanyCode 
/*[EXECUTABLE]*/                                                 ,INPUT  IntPol7072.Username      
/*[EXECUTABLE]*/                                                 ,INPUT  NO  
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_STKNo
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_docrun2
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_docrun3
/*[EXECUTABLE]*/                                                 ,OUTPUT nv_msgerror7072).
/*[BLANK]*/                                  
/*[EXECUTABLE]*/                         ASSIGN
/*[EXECUTABLE]*/                         IntPol7072.CMIBarCodeNumber = nv_STKNo
/*[EXECUTABLE]*/                         IntS7072.CMIBarCodeNumber   = nv_STKNo
/*[EXECUTABLE]*/                         IntPol7072.CMIDocumentUID   = nv_docno1
/*[EXECUTABLE]*/                         IntS7072.CMIDocumentUID     = nv_docno1 .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         OUTPUT TO PD_SAVEPD_01pin_BLKDOC72.TXT APPEND.
/*[EXECUTABLE]*/                         PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                             " PD_SAVEPD1_01: 72. Start: " FORMAT "x(25)"   SKIP.
/*[EXECUTABLE]*/                         PUT "company : " IntPol7072.CompanyCode FORMAT "x(5)"   
/*[EXECUTABLE]*/                             " nv_STdocno: " nv_STdocno FORMAT "x(15)"  
/*[EXECUTABLE]*/                             " nv_docno1: "  nv_docno1  FORMAT "x(15)" 
/*[EXECUTABLE]*/                             " nv_STKNo:  "  nv_STKNo   FORMAT "x(15)"  
/*[EXECUTABLE]*/                         SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                       ASSIGN
/*[EXECUTABLE]*/                       IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                       IntPol7072.GenSicBran     = NO
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranText = nv_msgerror7072
/*[EXECUTABLE]*/                       IntPol7072.GenSicBranST   = "ERROR"
/*[EXECUTABLE]*/                       IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[EXECUTABLE]*/                       IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                          IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[BLANK]*/                      
/*[EXECUTABLE]*/                         OUTPUT TO PD_SAVEPD_01pin_BLKDOC_ERR.TXT APPEND.
/*[EXECUTABLE]*/                         PUT "72 : ERROR"  IntPol7072.CompanyCode SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                       RETURN.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 IF SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q" THEN DO:
/*[EXECUTABLE]*/                   IF    (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND IntPol7072.DocumentUID = "")    /*nv_DocnoV70*/
/*[EXECUTABLE]*/                      OR (               IntPol7072.CMIPolicyTypeCd   <> ""  AND IntPol7072.CMIDocumentUID = "") /*nv_DocnoV72*/ 
/*[COMMENT]*/                         /**/
/*[EXECUTABLE]*/                      OR ( ( (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[EXECUTABLE]*/                      AND  nv_okprn = YES ) AND IntPol7072.DocumentUID = "")
/*[EXECUTABLE]*/                   THEN DO:
/*[EXECUTABLE]*/                     ASSIGN
/*[EXECUTABLE]*/                     IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                     IntPol7072.GenSicBran     = NO
/*[EXECUTABLE]*/                     IntPol7072.GenSicBranText = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº"
/*[EXECUTABLE]*/                     IntPol7072.GenSicBranST   = "ERROR"
/*[EXECUTABLE]*/                     IntPol7072.ErrorMessage   = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº".
/*[EXECUTABLE]*/                     IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                        IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[EXECUTABLE]*/                     RETURN.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END. 
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_02 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD1_02 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       70+72 
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /* CASE: 72  2.1 2.2 3.1 3.2 */
/*[EXECUTABLE]*/                 IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[EXECUTABLE]*/                     nv_msgerror7072 = "".
/*[EXECUTABLE]*/                     RUN UZ/UZO7201WS.P   /* create 72 */
/*[EXECUTABLE]*/                         (nv_RecIntPol7072
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                       NO-ERROR.
/*[EXECUTABLE]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                         OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                         PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                             TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                             " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.  /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
/*[COMMENT]*/                    /* CREATE : 70 */
/*[COMMENT]*/                    /*IF IntPol7072.PolicyTypeCd = "1"  THEN DO:*/
/*[EXECUTABLE]*/                 IF IntPol7072.PolicyTypeCd <> ""  THEN DO:    /*kridtiya i. */
/*[EXECUTABLE]*/                     RUN WRS/WRSGU100.P
/*[EXECUTABLE]*/                         (nv_RecIntPol7072
/*[EXECUTABLE]*/                          ,INPUT-OUTPUT nv_msgerror7072).
/*[EXECUTABLE]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                         OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                         PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                             TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                             " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                         OUTPUT CLOSE.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 IF nv_msgerror7072 <> "" THEN DO:
/*[EXECUTABLE]*/                     OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[EXECUTABLE]*/                     PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[EXECUTABLE]*/                         TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                         " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[EXECUTABLE]*/                     OUTPUT CLOSE.
/*[EXECUTABLE]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                         NO-ERROR.
/*[EXECUTABLE]*/                     IF AVAILABLE IntPol7072 THEN DO:
/*[EXECUTABLE]*/                         ASSIGN
/*[EXECUTABLE]*/                             IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[EXECUTABLE]*/                             IntPol7072.GenSicBran   = NO
/*[EXECUTABLE]*/                             IntPol7072.GenSicBranText = nv_msgerror7072
/*[EXECUTABLE]*/                             IntPol7072.GenSicBranST = "ERROR"
/*[EXECUTABLE]*/                             IntPol7072.ErrorMessage = nv_msgerror7072.
/*[EXECUTABLE]*/                         IF IntPol7072.DocumentUID <> "" THEN
/*[EXECUTABLE]*/                             IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                     RETURN.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_OLD C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD1_OLD :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_rec_rq    AS RECID     NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_PolicyV70 AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_PolicyV72 AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT-OUTPUT PARAMETER  nv_recinout7072   AS RECID     NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_trty11  AS CHARACTER NO-UNDO. /*M,T*/
/*[UNCALLED]*/                   DEFINE VARIABLE nv_docno1  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_STdocno AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_Renew   AS LOGICAL   NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_STKNo   AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE crCompanyNo AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE crBranchNo  AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_acno1 AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_agent AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   DEFINE VARIABLE nv_okprn AS LOGICAL   NO-UNDO.
/*[UNCALLED]*/                   /* ------------------------------------------------------ */
/*[UNCALLED]*/                   RUN PD_ConDBExp. /* DB EXPIRY */
/*[UNCALLED]*/                   /* ------------------------------------------------------ */
/*[UNCALLED]*/                   nv_resulttext = "".
/*[UNCALLED]*/                   FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[UNCALLED]*/                   /*¡ÃÁ¸ÃÃÁì»... + ¾Ãº. */
/*[UNCALLED]*/                   IF IntS7072.PolicyTypeCd <> "" AND IntS7072.RateGroup <> "" THEN DO:
/*[UNCALLED]*/                     nv_octets = "".
/*[UNCALLED]*/                     FIND FIRST IntPol7072 WHERE IntPol7072.PolicyNumber = nv_PolicyV70 /*70,72*/
/*[UNCALLED]*/                     NO-ERROR NO-WAIT.
/*[UNCALLED]*/                     IF NOT AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                       RUN WRS/WRSDigit.p (output nv_octets).
/*[UNCALLED]*/                       CREATE IntPol7072.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE nv_octets = IntPol7072.RqUID.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   ELSE DO:   /* ¡ÃÁ¸ÃÃÁì ¾Ãº.*/
/*[UNCALLED]*/                     IF IntS7072.CMIPolicyTypeCd <> "" AND IntS7072.CMIVehTypeCd <> "" THEN DO:
/*[UNCALLED]*/                       FIND FIRST IntPol7072 WHERE IntPol7072.CMIPolicyNumber = nv_PolicyV72
/*[UNCALLED]*/                       NO-ERROR NO-WAIT.
/*[UNCALLED]*/                       IF NOT AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                         nv_octets = "".
/*[UNCALLED]*/                         RUN WRS/WRSDigit.p (output nv_octets).
/*[UNCALLED]*/                         CREATE IntPol7072.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       ELSE nv_octets = IntPol7072.RqUID.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   nv_RecIntPol7072 = RECID(IntPol7072).
/*[UNCALLED]*/                   nv_recinout7072  = RECID(IntPol7072).
/*[UNCALLED]*/                   RUN PD_SAVEPD2.    /*Save data to IntPol7072*/
/*[UNCALLED]*/                   /* ----------------------------------------------------------------------- */
/*[UNCALLED]*/                   /* ËÒàÅ¢ Docno1 */
/*[UNCALLED]*/                   FIND IntS7072 WHERE RECID(IntS7072) = nv_rec_rq
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntS7072 THEN RETURN.
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[UNCALLED]*/                   /*20/3/2015*/
/*[UNCALLED]*/                   nv_okprn = NO.
/*[UNCALLED]*/                   RUN WRS\WRSFutil.p 
/*[UNCALLED]*/                       (IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                       ,IntPol7072.SumInsureAmt
/*[UNCALLED]*/                       ,IntPol7072.CompanyCode
/*[UNCALLED]*/                       ,INPUT-OUTPUT nv_okprn).
/*[UNCALLED]*/                   /* ---------------------------------------------------- */
/*[UNCALLED]*/                   IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3"
/*[UNCALLED]*/                      OR                IntPol7072.CMIPolicyTypeCd    <> ""
/*[UNCALLED]*/                      OR  (IntPol7072.CompanyCode = "833" AND 
/*[UNCALLED]*/                          (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[UNCALLED]*/                      AND nv_okprn = YES )
/*[UNCALLED]*/                   /**AND (IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2")
/*[UNCALLED]*/                      AND DECIMAL(IntPol7072.SumInsureAmt) <= 200000) */
/*[UNCALLED]*/                   THEN DO:
/*[UNCALLED]*/                     IF     IntS7072.DocumentUID      = "" /*nv_DocnoV70*/
/*[UNCALLED]*/                        AND IntS7072.CMIDocumentUID   = "" /*nv_DocnoV72*/
/*[UNCALLED]*/                        AND IntS7072.StickerNumber    = ""
/*[UNCALLED]*/                        AND IntS7072.CMIBarCodeNumber = ""
/*[UNCALLED]*/                     THEN DO:
/*[UNCALLED]*/                       ASSIGN nv_trty11 = "" nv_docno1 = "" nv_STdocno = "".
/*[UNCALLED]*/                       IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1)  = "3"
/*[UNCALLED]*/                          OR (IntPol7072.CompanyCode = "833" AND 
/*[UNCALLED]*/                        (     IntPol7072.PolicyTypeCd = "2.1" OR IntPol7072.PolicyTypeCd = "2.2"
/*[UNCALLED]*/                          OR  IntPol7072.PolicyTypeCd = "2.3" OR IntPol7072.PolicyTypeCd = "2.4"
/*[UNCALLED]*/                          OR  IntPol7072.PolicyTypeCd = "2.5" OR IntPol7072.PolicyTypeCd = "2.6" ) )
/*[UNCALLED]*/                       THEN DO:
/*[UNCALLED]*/                         FIND FIRST FUtilSetUp WHERE
/*[UNCALLED]*/                              FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[UNCALLED]*/                          AND FUtilSetUp.EffDate    <= TODAY
/*[UNCALLED]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                         IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[UNCALLED]*/                         ELSE DO:
/*[UNCALLED]*/                           FIND FIRST FUtilSetUp WHERE
/*[UNCALLED]*/                                FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp1 = "V70"      /*V70,V72*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp2 = IntPol7072.PolicyTypeCd  /*3,3.1,3.2,T*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[UNCALLED]*/                            AND FUtilSetUp.EffDate    <= TODAY
/*[UNCALLED]*/                           NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         IF nv_STdocno = "YES" THEN DO:
/*[UNCALLED]*/                           nv_trty11       = "M".
/*[UNCALLED]*/                           nv_msgerror7072 = "".
/*[UNCALLED]*/                           RUN WSP/WSPFRecp.p
/*[UNCALLED]*/                               (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[UNCALLED]*/                               ,nv_trty11
/*[UNCALLED]*/                               ,OUTPUT nv_docno1
/*[UNCALLED]*/                               ,OUTPUT nv_STKNo
/*[UNCALLED]*/                               ,OUTPUT nv_msgerror7072
/*[UNCALLED]*/                               ).
/*[UNCALLED]*/                           ASSIGN
/*[UNCALLED]*/                           IntPol7072.DocumentUID = nv_docno1
/*[UNCALLED]*/                           IntS7072.DocumentUID   = nv_docno1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                         ASSIGN
/*[UNCALLED]*/                         IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[UNCALLED]*/                         IntPol7072.GenSicBran     = NO
/*[UNCALLED]*/                         IntPol7072.GenSicBranText = nv_msgerror7072
/*[UNCALLED]*/                         IntPol7072.GenSicBranST   = "ERROR"
/*[UNCALLED]*/                         IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[UNCALLED]*/                         IF IntPol7072.DocumentUID <> "" THEN
/*[UNCALLED]*/                            IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[UNCALLED]*/                         RETURN.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[UNCALLED]*/                         FIND FIRST FUtilSetUp WHERE
/*[UNCALLED]*/                              FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[UNCALLED]*/                          AND FUtilSetUp.KeyUtilGrp5 = "ALL"  /*833*/
/*[UNCALLED]*/                          AND FUtilSetUp.EffDate    <= TODAY
/*[UNCALLED]*/                         NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                         IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[UNCALLED]*/                         ELSE DO:
/*[UNCALLED]*/                           FIND FIRST FUtilSetUp WHERE
/*[UNCALLED]*/                                FUtilSetUp.UtilGrp     = "UTGRP01"  /*UtGrp01*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp1 = "V72"      /*V70,V72*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp2 = "T"        /*3,3.1,3.2,T*/
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp3 = "DOC"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp4 = "BLK"
/*[UNCALLED]*/                            AND FUtilSetUp.KeyUtilGrp5 = IntPol7072.CompanyCode
/*[UNCALLED]*/                            AND FUtilSetUp.EffDate    <= TODAY
/*[UNCALLED]*/                           NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                           IF AVAILABLE FUtilSetUp THEN nv_STdocno = FUtilSetUp.UtilGrpCd1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         IF nv_STdocno = "YES" THEN DO:
/*[UNCALLED]*/                           nv_trty11       = "T".
/*[UNCALLED]*/                           nv_msgerror7072 = "".
/*[UNCALLED]*/                           RUN WSP/WSPFRecp.p
/*[UNCALLED]*/                               (IntPol7072.CompanyCode /*nv_CompanyNo*/
/*[UNCALLED]*/                               ,nv_trty11
/*[UNCALLED]*/                               ,OUTPUT nv_docno1
/*[UNCALLED]*/                               ,OUTPUT nv_STKNo
/*[UNCALLED]*/                               ,OUTPUT nv_msgerror7072
/*[UNCALLED]*/                               ).
/*[UNCALLED]*/                           ASSIGN
/*[UNCALLED]*/                           IntPol7072.CMIBarCodeNumber = nv_STKNo
/*[UNCALLED]*/                           IntS7072.CMIBarCodeNumber   = nv_STKNo
/*[UNCALLED]*/                           IntPol7072.CMIDocumentUID = nv_docno1
/*[UNCALLED]*/                           IntS7072.CMIDocumentUID   = nv_docno1 .
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                         ASSIGN
/*[UNCALLED]*/                         IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[UNCALLED]*/                         IntPol7072.GenSicBran     = NO
/*[UNCALLED]*/                         IntPol7072.GenSicBranText = nv_msgerror7072
/*[UNCALLED]*/                         IntPol7072.GenSicBranST   = "ERROR"
/*[UNCALLED]*/                         IntPol7072.ErrorMessage   = nv_msgerror7072.
/*[UNCALLED]*/                         IF IntPol7072.DocumentUID <> "" THEN
/*[UNCALLED]*/                            IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[UNCALLED]*/                         RETURN.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   /**/
/*[UNCALLED]*/                   IF SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q" THEN DO:
/*[UNCALLED]*/                     IF    (SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3" AND IntPol7072.DocumentUID = "") /*nv_DocnoV70*/
/*[UNCALLED]*/                        OR (               IntPol7072.CMIPolicyTypeCd   <> ""  AND IntPol7072.CMIDocumentUID = "") /*nv_DocnoV72*/ 
/*[UNCALLED]*/                        /**/
/*[UNCALLED]*/                        OR ( ( (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[UNCALLED]*/                        AND  nv_okprn = YES ) AND IntPol7072.DocumentUID = "")
/*[UNCALLED]*/                     THEN DO:
/*[UNCALLED]*/                       ASSIGN
/*[UNCALLED]*/                       IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[UNCALLED]*/                       IntPol7072.GenSicBran     = NO
/*[UNCALLED]*/                       IntPol7072.GenSicBranText = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº"
/*[UNCALLED]*/                       IntPol7072.GenSicBranST   = "ERROR"
/*[UNCALLED]*/                       IntPol7072.ErrorMessage   = "äÁè¾ºàÅ¢·ÕèàÍ¡ÊÒÃã¹ÃÐºº".
/*[UNCALLED]*/                       IF IntPol7072.DocumentUID <> "" THEN
/*[UNCALLED]*/                          IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[UNCALLED]*/                       RETURN.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   /* ----------------------------------------------------------------------- */
/*[UNCALLED]*/                   nv_Renew = NO.  /*A57-0300: §Ò¹µèÍÍÒÂØ*/
/*[UNCALLED]*/                   OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[UNCALLED]*/                   PUT "PD_SAVEPD1: 1. START " FORMAT "x(25)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                   SKIP.
/*[UNCALLED]*/                   OUTPUT CLOSE.
/*[UNCALLED]*/                   IF IntPol7072.Rencnt <> 0 
/*[UNCALLED]*/                      AND SUBSTR(IntPol7072.PolicyNumber,1,1) =  "R"
/*[UNCALLED]*/                      AND IntPol7072.PreviousPolicyNumber     <> ""
/*[UNCALLED]*/                   THEN nv_Renew = YES.
/*[UNCALLED]*/                   nv_msgerror7072  = "".
/*[UNCALLED]*/                   /* Generate policy, uwm100, uwm120, uwm301, uwm130, uwd132 */
/*[UNCALLED]*/                   IF     IntPol7072.PolicyTypeCd <> "1" 
/*[UNCALLED]*/                      AND IntPol7072.PolicyTypeCd <> "2"
/*[UNCALLED]*/                      AND IntPol7072.PolicyTypeCd <> "3"
/*[UNCALLED]*/                   THEN DO:
/*[UNCALLED]*/                     IF    IntPol7072.PolicyTypeCd = ""
/*[UNCALLED]*/                        OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[UNCALLED]*/                        OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[UNCALLED]*/                        /**/
/*[UNCALLED]*/                        OR IntPol7072.CMIPolicyTypeCd = "110"
/*[UNCALLED]*/                        OR IntPol7072.CMIPolicyTypeCd = "120A"
/*[UNCALLED]*/                        OR IntPol7072.CMIPolicyTypeCd = "140A"
/*[UNCALLED]*/                     THEN DO:
/*[UNCALLED]*/                       IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[UNCALLED]*/                         nv_msgerror7072 = "".
/*[UNCALLED]*/                         RUN UZ/UZO7201WS.P
/*[UNCALLED]*/                              (nv_RecIntPol7072
/*[UNCALLED]*/                              ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                         FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                         NO-ERROR.
/*[UNCALLED]*/                         IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                           OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[UNCALLED]*/                           PUT "2. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[UNCALLED]*/                               TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                               " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[UNCALLED]*/                           OUTPUT CLOSE.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END. /*IF  IntPol7072.CMIPolicyTypeCd <> "" THEN DO:*/
/*[UNCALLED]*/                       IF    SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "2"
/*[UNCALLED]*/                          OR SUBSTRING(TRIM(IntPol7072.PolicyTypeCd),1,1) = "3"
/*[UNCALLED]*/                       THEN DO:
/*[UNCALLED]*/                         RUN WRS/WRSGU100.P
/*[UNCALLED]*/                              (nv_RecIntPol7072
/*[UNCALLED]*/                              ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                         IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                           OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[UNCALLED]*/                           PUT "3. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[UNCALLED]*/                               TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                               " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[UNCALLED]*/                           OUTPUT CLOSE.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       IF IntPol7072.CMIPolicyTypeCd <> "" THEN DO:
/*[UNCALLED]*/                         RUN UZ/UZO7201WS.P
/*[UNCALLED]*/                              (nv_RecIntPol7072
/*[UNCALLED]*/                              ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                         FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                         NO-ERROR.
/*[UNCALLED]*/                         IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                           OUTPUT TO WSPGP100-ERROR.TXT APPEND.
/*[UNCALLED]*/                           PUT "4. UZO7201WS.P " IntPol7072.CMIPolicyNumber FORMAT "X(18)"
/*[UNCALLED]*/                               TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                               " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[UNCALLED]*/                           OUTPUT CLOSE.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   ELSE DO:
/*[UNCALLED]*/                     /* A57-0300: §Ò¹µèÍÍÒÂØ */
/*[UNCALLED]*/                     IF nv_Renew = YES THEN DO:
/*[UNCALLED]*/                       IF NOT CONNECTED ("expiry") THEN DO:
/*[UNCALLED]*/                         RUN WRS/WRSGU1DB.P
/*[UNCALLED]*/                              (""  /*Userid*/
/*[UNCALLED]*/                             ,""  /*Password*/
/*[UNCALLED]*/                             ,"expiry"). /*Database name*/
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       IF CONNECTED ("expiry") THEN DO:
/*[UNCALLED]*/                         RUN WRS/WRSGU10R.P
/*[UNCALLED]*/                           (nv_RecIntPol7072
/*[UNCALLED]*/                           ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       ELSE  nv_msgerror7072 = "Not Connect Expiry: ".
/*[UNCALLED]*/                       /**/
/*[UNCALLED]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                       NO-ERROR.
/*[UNCALLED]*/                       IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                         OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[UNCALLED]*/                         PUT "5. WRSGU10R.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[UNCALLED]*/                             TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                             " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[UNCALLED]*/                         OUTPUT CLOSE.
/*[UNCALLED]*/                         nv_msgerror7072 = "".
/*[UNCALLED]*/                         RUN WRS/WRSGU100.P
/*[UNCALLED]*/                            (nv_RecIntPol7072
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                       NO-ERROR.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                       IF IntPol7072.PolicyTypeCd <> "1"  THEN DO:
/*[UNCALLED]*/                         RUN WRS/WRSGU100.P
/*[UNCALLED]*/                            (nv_RecIntPol7072
/*[UNCALLED]*/                            ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" AND IntPol7072.PolicyTypeCd = "1" THEN DO:
/*[UNCALLED]*/                           nv_acno1 = "".
/*[UNCALLED]*/                           /* ËÒ ÃËÑÊµÑÇá·¹  */
/*[UNCALLED]*/                           RUN WSP/WSPMCpny.P 
/*[UNCALLED]*/                               (IntPol7072.CompanyCode
/*[UNCALLED]*/                               ,IntPol7072.BranchCd
/*[UNCALLED]*/                               ,"V" + SUBSTR(IntPol7072.PolicyNumber,3,2) /*nv_Poltyp*/
/*[UNCALLED]*/                               ,OUTPUT crCompanyNo
/*[UNCALLED]*/                               ,OUTPUT crBranchNo
/*[UNCALLED]*/                               ,OUTPUT nv_acno1
/*[UNCALLED]*/                               ,OUTPUT nv_agent
/*[UNCALLED]*/                               ,OUTPUT nv_msgerror7072
/*[UNCALLED]*/                               ).
/*[UNCALLED]*/                           /**/
/*[UNCALLED]*/                           IF nv_acno1 <> "" THEN DO:
/*[UNCALLED]*/                             FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                             NO-ERROR.
/*[UNCALLED]*/                             IF AVAILABLE IntPol7072 THEN 
/*[UNCALLED]*/                               IntPol7072.AgentBrokerLicenseNumber = nv_acno1.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         ELSE DO:
/*[UNCALLED]*/                           RUN WRS/WRSGU100.P
/*[UNCALLED]*/                               (nv_RecIntPol7072
/*[UNCALLED]*/                               ,INPUT-OUTPUT nv_msgerror7072).
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                       OUTPUT TO WRSGU100-ERROR.TXT APPEND.
/*[UNCALLED]*/                       PUT "6. WRSGU100.P " IntPol7072.PolicyNumber FORMAT "X(18)"
/*[UNCALLED]*/                           TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                           " " nv_msgerror7072 FORMAT "X(150)" SKIP.
/*[UNCALLED]*/                       OUTPUT CLOSE.
/*[UNCALLED]*/                       FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                       NO-ERROR.
/*[UNCALLED]*/                       IF AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                         ASSIGN
/*[UNCALLED]*/                         IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[UNCALLED]*/                         IntPol7072.GenSicBran   = NO
/*[UNCALLED]*/                         IntPol7072.GenSicBranText = nv_msgerror7072
/*[UNCALLED]*/                         IntPol7072.GenSicBranST = "ERROR"
/*[UNCALLED]*/                         IntPol7072.ErrorMessage = nv_msgerror7072.
/*[UNCALLED]*/                         IF IntPol7072.DocumentUID <> "" THEN
/*[UNCALLED]*/                            IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       RETURN.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   OUTPUT TO PD_SAVEPD1.TXT APPEND.
/*[UNCALLED]*/                   PUT "PD_SAVEPD1: 2. END " FORMAT "x(25)"
/*[UNCALLED]*/                       " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                   SKIP.
/*[UNCALLED]*/                   OUTPUT CLOSE.
/*[UNCALLED]*/                   /* ------------------------------------------------------ */
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                   NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                     ASSIGN
/*[UNCALLED]*/                     IntPol7072.GenSicBran   = YES
/*[UNCALLED]*/                     IntPol7072.GenSicBranBy = "WRSBQ7072.W"
/*[UNCALLED]*/                     IntPol7072.GenSicBranDt = TODAY
/*[UNCALLED]*/                     IntPol7072.GenSicBranTime = STRING(TIME,"HH:MM:SS")
/*[UNCALLED]*/                     IntPol7072.GenSicBranText = ""
/*[UNCALLED]*/                     IntPol7072.GenSicBranST = "".
/*[UNCALLED]*/                     IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN IntPol7072.TransferToPremium = YES. /*kridtiya i.*/
/*[UNCALLED]*/                     IF nv_msgerror7072 <> "" THEN DO:
/*[UNCALLED]*/                       ASSIGN
/*[UNCALLED]*/                       IntPol7072.ContractNumber = TRIM(IntPol7072.ContractNumber) + "CA"
/*[UNCALLED]*/                       IntPol7072.GenSicBran   = NO
/*[UNCALLED]*/                       IntPol7072.GenSicBranText = nv_msgerror7072
/*[UNCALLED]*/                       IntPol7072.GenSicBranST = "ERROR"
/*[UNCALLED]*/                       IntPol7072.ErrorMessage = nv_msgerror7072.
/*[UNCALLED]*/                       IF IntPol7072.DocumentUID <> "" THEN
/*[UNCALLED]*/                          IntPol7072.DocumentUID = TRIM(IntPol7072.DocumentUID) + "CA".
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                   END.
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[UNCALLED]*/                   NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF AVAILABLE IntPol7072 THEN DO:
/*[UNCALLED]*/                     IF IntPol7072.ConfirmBy <> "AUTO" THEN DO:
/*[UNCALLED]*/                       /*»2.1 2.2 ·Ø¹µèÓ¡ÇèÒ1áÊ¹ÍÍ¡¡ÃÁ¸ÃÃÁì D ËÃ×ÍàÅ¢ÊÒ¢Ò 2 ËÅÑ¡*/
/*[UNCALLED]*/                       IF (SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "R" AND SUBSTRING(IntPol7072.PolicyNumber,1,1) <> "Q")
/*[UNCALLED]*/                          AND  nv_okprn = YES
/*[UNCALLED]*/                       THEN DO:
/*[UNCALLED]*/                         /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[UNCALLED]*/                         RUN PD_SAVEPD1FileAttach (nv_RecIntPol7072
/*[UNCALLED]*/                                                  ,nv_rec_rq ).
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                       ELSE DO:
/*[UNCALLED]*/                         OUTPUT TO ConfirmBy.TXT.
/*[UNCALLED]*/                         PUT IntPol7072.ConfirmBy SKIP.
/*[UNCALLED]*/                         OUTPUT CLOSE.
/*[UNCALLED]*/                         IF IntPol7072.ChkVehicle = YES THEN DO:
/*[UNCALLED]*/                           nv_resulttext = "Êè§µÃÇ¨ÊÀÒ¾Ã¶".
/*[UNCALLED]*/                           nv_cvehtext =
/*[UNCALLED]*/                             "µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)
/*[UNCALLED]*/                           + " ¡ÃÁ¸ÃÃÁì¾Ãº.: " + TRIM(nv_PolicyV72)
/*[UNCALLED]*/                             + " ·ÐàºÕÂ¹Ã¶ : " + TRIM(IntPol7072.Registration) 
/*[UNCALLED]*/                                         + " " + TRIM(IntPol7072.RegisteredProvCd)
/*[UNCALLED]*/                           + " ¼ÙéàÍÒ»ÃÐ¡Ñ¹ÀÑÂ : " + TRIM(TRIM(TRIM(IntPol7072.InsuredTitle)
/*[UNCALLED]*/                           + " " + TRIM(IntPol7072.InsuredName))
/*[UNCALLED]*/                           + " " + TRIM(IntPol7072.InsuredSurname)).
/*[UNCALLED]*/                           OUTPUT TO ChkVehicle.TXT.
/*[UNCALLED]*/                           PUT " ConfirmBy:" IntPol7072.ConfirmBy  SKIP.
/*[UNCALLED]*/                           PUT "ChkVehicle:" IntPol7072.ChkVehicle SKIP.
/*[UNCALLED]*/                           PUT "           " nv_resulttext FORMAT "X(50)" SKIP.
/*[UNCALLED]*/                           PUT "           " nv_cvehtext   FORMAT "X(200)" SKIP.
/*[UNCALLED]*/                           PUT "ChkVehSend:" IntPol7072.ChkVehSend SKIP.
/*[UNCALLED]*/                           PUT "   Mail to:" IntPol7072.ChkVehMail SKIP.
/*[UNCALLED]*/                           PUT "------------------------------------------------" FORMAT "X(50)" SKIP.
/*[UNCALLED]*/                           PUT "ChkVehAssignSend: " IntPol7072.ChkVehAssignSend SKIP.
/*[UNCALLED]*/                           PUT "ChkVehAssignMail: " IntPol7072.ChkVehAssignMail SKIP.
/*[UNCALLED]*/                           OUTPUT CLOSE.
/*[UNCALLED]*/                           IF IntPol7072.ChkVehSend = YES THEN DO:
/*[UNCALLED]*/                             IF IntPol7072.ChkVehMail <> "" THEN DO:
/*[UNCALLED]*/                               RUN GW/gwtomail 
/*[UNCALLED]*/                                 (IntPol7072.ChkVehMail /*To "USERNAME"*/
/*[UNCALLED]*/                                 ,""          /*CC.*/
/*[UNCALLED]*/                                 ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70)) /*Subject: WARNING VIB*/
/*[UNCALLED]*/                                 ,nv_cvehtext /*Body*/
/*[UNCALLED]*/                                 ).
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           IF IntPol7072.ChkVehAssignSend = YES THEN DO:
/*[UNCALLED]*/                             IF IntPol7072.ChkVehAssignMail <> "" THEN DO:
/*[UNCALLED]*/                               RUN GW/gwtomail
/*[UNCALLED]*/                                 (IntPol7072.ChkVehAssignMail
/*[UNCALLED]*/                                 ,""
/*[UNCALLED]*/                                 ,("WARNING: µÃÇ¨ÊÀÒ¾Ã¶ ¡ÃÁ¸ÃÃÁì: " + TRIM(nv_PolicyV70))
/*[UNCALLED]*/                                 ,nv_cvehtext
/*[UNCALLED]*/                                 ).
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                           IF IntPol7072.TransferToPremium = YES THEN DO: /* Gen Uwm100 to DB GWCtx */
/*[UNCALLED]*/                             RUN WRS\WRSGwCtx 
/*[UNCALLED]*/                                 (IntPol7072.PolicyNumber
/*[UNCALLED]*/                                 ,IntPol7072.Rencnt
/*[UNCALLED]*/                                 ,IntPol7072.Endcnt
/*[UNCALLED]*/                                 ,0  /*RECID(uwm100)*/
/*[UNCALLED]*/                                 ,IntPol7072.CompanyCode  /*833*/
/*[UNCALLED]*/                                 ,IntPol7072.PolicyTypeCd /*2.2*/
/*[UNCALLED]*/                                 ,IntPol7072.RateGroup).  /*110*/
/*[UNCALLED]*/                           END.
/*[UNCALLED]*/                         END. /*IF IntS7072.ChkVehicle = YES*/
/*[UNCALLED]*/                       END. 
/*[UNCALLED]*/                     END. /*IF IntS7072.ConfirmBy <> "AUTO" */
/*[UNCALLED]*/                     IF IntPol7072.ConfirmBy = "AUTO" THEN DO:
/*[UNCALLED]*/                         IF SUBSTRING(IntPol7072.PolicyNumber,1,1) = "R" THEN 
/*[UNCALLED]*/                         RUN WRS\WRSGwCtx 
/*[UNCALLED]*/                                 (IntPol7072.PolicyNumber
/*[UNCALLED]*/                                 ,IntPol7072.Rencnt
/*[UNCALLED]*/                                 ,IntPol7072.Endcnt
/*[UNCALLED]*/                                 ,0  /*RECID(uwm100)*/
/*[UNCALLED]*/                                 ,IntPol7072.CompanyCode  /*833*/
/*[UNCALLED]*/                                 ,IntPol7072.PolicyTypeCd /*2.2*/
/*[UNCALLED]*/                                 ,IntPol7072.RateGroup).  /*110*/ /*kridtiya i.*/
/*[UNCALLED]*/                         ELSE  /*¾ÔÁ¾ì áÅÐá¹º file Êè§¡ÅÑº*/
/*[UNCALLED]*/                            RUN PD_SAVEPD1FileAttach
/*[UNCALLED]*/                               (nv_RecIntPol7072
/*[UNCALLED]*/                               ,nv_rec_rq ).  /*RECID(IntS7072)*/
/*[UNCALLED]*/                       END.
/*[UNCALLED]*/                   END.  /*IF AVAILABLE IntPol7072 */
/*[UNCALLED]*/                   RELEASE IntS7072.
/*[UNCALLED]*/                   RELEASE IntPol7072.
/*[UNCALLED]*/                   RELEASE IntQPolicy.
/*[UNCALLED]*/                   /**/   */
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD1_R C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEPD1_R :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   RUN WRS\WRSGwCtx 
/*[UNCALLED]*/                                 (IntPol7072.PolicyNumber
/*[UNCALLED]*/                                 ,IntPol7072.Rencnt
/*[UNCALLED]*/                                 ,IntPol7072.Endcnt
/*[UNCALLED]*/                                 ,0  /*RECID(uwm100)*/
/*[UNCALLED]*/                                 ,IntPol7072.CompanyCode  /*833*/
/*[UNCALLED]*/                                 ,IntPol7072.PolicyTypeCd /*2.2*/
/*[UNCALLED]*/                                 ,IntPol7072.RateGroup).  /*110*/
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD2 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD2 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes: PD_SAVEPD1 Êè§ÁÒ      
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 OUTPUT TO PD_SAVEPD2.TXT APPEND.
/*[EXECUTABLE]*/                 PUT "PD_SAVEPD2: 1. Start: " FORMAT "x(25)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                 SKIP.
/*[EXECUTABLE]*/                 OUTPUT CLOSE.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RecIntPol7072
/*[EXECUTABLE]*/                 NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                 IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntPol7072.Username          = IntS7072.Username
/*[EXECUTABLE]*/                 IntPol7072.Password          = IntS7072.Password
/*[EXECUTABLE]*/                 IntPol7072.CompanyCode       = IntS7072.CompanyCode
/*[EXECUTABLE]*/                 IntPol7072.BranchCd          = IntS7072.BranchCd
/*[EXECUTABLE]*/                 IntPol7072.InsurerId         = IntS7072.InsurerId
/*[EXECUTABLE]*/                 IntPol7072.InsuranceCd       = IntS7072.InsuranceCd
/*[EXECUTABLE]*/                 IntPol7072.InsuranceBranchCd = IntS7072.InsuranceBranchCd
/*[EXECUTABLE]*/                 IntPol7072.PolicyStatus      = IntS7072.PolicyStatus
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.PolicyNumber      = nv_PolicyV70
/*[COMMENT]*/                    /*IntPol7072.DocumentUID       = IntS7072.DocumentUID /*nv_DocnoV70*/*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.PreviousPolicyNumber = IntS7072.PreviousPolicyNumber
/*[EXECUTABLE]*/                 IntPol7072.StickerNumber        = IntS7072.StickerNumber
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.ContractNumber           = IntS7072.ContractNumber
/*[EXECUTABLE]*/                 IntPol7072.ContractDt               = IntS7072.ContractDt
/*[EXECUTABLE]*/                 IntPol7072.ContractTime             = IntS7072.ContractTime
/*[EXECUTABLE]*/                 IntPol7072.CMVApplicationNumber     = IntS7072.CMVApplicationNumber /*QNumPremium*/
/*[EXECUTABLE]*/                 IntPol7072.ApplicationNumber        = IntS7072.ApplicationNumber
/*[EXECUTABLE]*/                 IntPol7072.ApplicationDt            = IntS7072.ApplicationDt
/*[EXECUTABLE]*/                 IntPol7072.ApplicationTime          = IntS7072.ApplicationTime
/*[EXECUTABLE]*/                 IntPol7072.AgreeDt                  = IntS7072.AgreeDt
/*[EXECUTABLE]*/                 IntPol7072.IssueDt                  = IntS7072.IssueDt
/*[EXECUTABLE]*/                 IntPol7072.EffectiveDt              = IntS7072.EffectiveDt
/*[EXECUTABLE]*/                 IntPol7072.ExpirationDt             = IntS7072.ExpirationDt
/*[EXECUTABLE]*/                 IntPol7072.EndDt                    = IntS7072.EndDt
/*[EXECUTABLE]*/                 IntPol7072.SetTime                  = IntS7072.SetTime
/*[EXECUTABLE]*/                 IntPol7072.VehicleTypeCode          = IntS7072.VehicleTypeCode
/*[EXECUTABLE]*/                 IntPol7072.VehTypeCd                = IntS7072.VehTypeCd
/*[EXECUTABLE]*/                 IntPol7072.RegisteredProvinceCode   = IntS7072.RegisteredProvinceCode
/*[EXECUTABLE]*/                 IntPol7072.PlateNumber              = IntS7072.PlateNumber
/*[EXECUTABLE]*/                 IntPol7072.Registration             = IntS7072.Registration
/*[EXECUTABLE]*/                 IntPol7072.RegisteredProvCd         = IntS7072.RegisteredProvCd
/*[EXECUTABLE]*/                 IntPol7072.VehGroup                 = IntS7072.VehGroup
/*[EXECUTABLE]*/                 IntPol7072.Manufacturer             = IntS7072.Manufacturer
/*[EXECUTABLE]*/                 IntPol7072.Model                    = IntS7072.Model
/*[EXECUTABLE]*/                 IntPol7072.ModelTypeName            = IntS7072.ModelTypeName
/*[EXECUTABLE]*/                 IntPol7072.VehBodyTypeDesc          = IF       index(IntS7072.VehBodyTypeDesc,"null")  <> 0 THEN "" 
/*[EXECUTABLE]*/                                                       ELSE IF  index(IntS7072.VehBodyTypeDesc,"äÁèÁÕ") <> 0 THEN ""
/*[EXECUTABLE]*/                                                       ELSE    IntS7072.VehBodyTypeDesc  
/*[EXECUTABLE]*/                 IntPol7072.ColourCd                 = IntS7072.ColourCd
/*[EXECUTABLE]*/                 IntPol7072.Colour                   = IntS7072.Colour
/*[EXECUTABLE]*/                 IntPol7072.ModelYear                = IntS7072.ModelYear
/*[EXECUTABLE]*/                 IntPol7072.Displacement             = IF IntS7072.Displacement             = "null" THEN "0" ELSE IntS7072.Displacement
/*[EXECUTABLE]*/                 IntPol7072.GrossVehOrCombinedWeight = IF IntS7072.GrossVehOrCombinedWeight = "null" THEN "0" ELSE IntS7072.GrossVehOrCombinedWeight
/*[EXECUTABLE]*/                 IntPol7072.SeatingCapacity          = IF IntS7072.SeatingCapacity          = "null" THEN "0" ELSE IntS7072.SeatingCapacity 
/*[EXECUTABLE]*/                 IntPol7072.ChassisVINNumber         = IntS7072.ChassisVINNumber
/*[EXECUTABLE]*/                 IntPol7072.ChassisSerialNumber      = IntS7072.ChassisSerialNumber
/*[EXECUTABLE]*/                 IntPol7072.EngineSerialNumber       = IntS7072.EngineSerialNumber
/*[EXECUTABLE]*/                 IntPol7072.RegisteredYear           = IntS7072.RegisteredYear
/*[EXECUTABLE]*/                 IntPol7072.License                  = IntS7072.License
/*[EXECUTABLE]*/                 IntPol7072.InsuredType              = IntS7072.InsuredType
/*[EXECUTABLE]*/                 IntPol7072.InsuredUniqueID          = IntS7072.InsuredUniqueID
/*[EXECUTABLE]*/                 IntPol7072.InsuredUniqueIDExpDt     = IntS7072.InsuredUniqueIDExpDt
/*[EXECUTABLE]*/                 IntPol7072.InsuredUniqueIDExpirationDt = IntS7072.InsuredUniqueIDExpirationDt
/*[EXECUTABLE]*/                 IntPol7072.BirthDt           = IF index(IntS7072.BirthDt,"null") = 0 THEN  IntS7072.BirthDt ELSE ""
/*[EXECUTABLE]*/                 IntPol7072.InsuredCd         = IntS7072.InsuredCd
/*[COMMENT]*/                    /*IntPol7072.InsuredTitle      = IntS7072.InsuredTitle  Comment by 
/*[COMMENT]*/                    IntPol7072.InsuredTitle      =  IF      trim(IntS7072.InsuredTitle) = "¹ÒÂ"    THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹Ò§ÊÒÇ" THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹Ò§"    THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE IF trim(IntS7072.InsuredTitle) = "¹.Ê."   THEN "¤Ø³"   /*comment by Kridtiya I. */
/*[COMMENT]*/                                                    ELSE    trim(IntS7072.InsuredTitle)*/
/*[EXECUTABLE]*/                 IntPol7072.InsuredTitle      = IntS7072.InsuredTitle     /*Add by kridtiya i . Date. 03/09/2018*/
/*[EXECUTABLE]*/                 IntPol7072.InsuredName       = IntS7072.InsuredName
/*[EXECUTABLE]*/                 IntPol7072.InsuredSurname    = IntS7072.InsuredSurname
/*[EXECUTABLE]*/                 IntPol7072.InsuredBranch     = IntS7072.InsuredBranch
/*[EXECUTABLE]*/                 IntPol7072.Addr              = IntS7072.Addr
/*[EXECUTABLE]*/                 IntPol7072.UnitNumber        = IntS7072.UnitNumber
/*[EXECUTABLE]*/                 IntPol7072.RoomNumber        = IntS7072.RoomNumber
/*[EXECUTABLE]*/                 IntPol7072.Building          = IntS7072.Building
/*[EXECUTABLE]*/                 IntPol7072.VillageNumber     = IntS7072.VillageNumber
/*[EXECUTABLE]*/                 IntPol7072.Alley             = IntS7072.Alley
/*[EXECUTABLE]*/                 IntPol7072.Lane              = IntS7072.Lane
/*[EXECUTABLE]*/                 IntPol7072.StreetName        = IntS7072.StreetName
/*[EXECUTABLE]*/                 IntPol7072.SubDistrict       = IntS7072.SubDistrict
/*[EXECUTABLE]*/                 IntPol7072.District          = IntS7072.District
/*[EXECUTABLE]*/                 IntPol7072.StateProvCd       = IntS7072.StateProvCd
/*[EXECUTABLE]*/                 IntPol7072.StateProv         = IntS7072.StateProv
/*[EXECUTABLE]*/                 IntPol7072.Province          = IntS7072.Province
/*[EXECUTABLE]*/                 IntPol7072.PostalCode        = IntS7072.PostalCode
/*[EXECUTABLE]*/                 IntPol7072.OccupationDesc    = IntS7072.OccupationDesc
/*[EXECUTABLE]*/                 IntPol7072.MobilePhoneNumber = IntS7072.MobilePhoneNumber
/*[EXECUTABLE]*/                 IntPol7072.MobileNumber      = IntS7072.MobileNumber
/*[EXECUTABLE]*/                 IntPol7072.PhoneNumber       = IntS7072.PhoneNumber
/*[EXECUTABLE]*/                 IntPol7072.OfficePhoneNumber = IntS7072.OfficePhoneNumber
/*[EXECUTABLE]*/                 IntPol7072.EmailAddr         = IntS7072.EmailAddr
/*[EXECUTABLE]*/                 IntPol7072.WrittenAmt        = IntS7072.WrittenAmt
/*[EXECUTABLE]*/                 IntPol7072.RevenueStampAmt   = IntS7072.RevenueStampAmt
/*[EXECUTABLE]*/                 IntPol7072.VatAmt            = IntS7072.VatAmt
/*[EXECUTABLE]*/                 IntPol7072.CurrentTermAmt    = IntS7072.CurrentTermAmt
/*[EXECUTABLE]*/                 IntPol7072.RegisterDt        = IntS7072.RegisterDt
/*[EXECUTABLE]*/                 IntPol7072.ShowroomID        = IntS7072.ShowroomID
/*[EXECUTABLE]*/                 IntPol7072.ShowroomName      = IntS7072.ShowroomName
/*[EXECUTABLE]*/                 IntPol7072.ReceiptDt         = IntS7072.ReceiptDt
/*[EXECUTABLE]*/                 IntPol7072.AgencyEmployee    = IntS7072.AgencyEmployee
/*[EXECUTABLE]*/                 IntPol7072.RemarkText        = IntS7072.RemarkText
/*[EXECUTABLE]*/                 IntPol7072.ReceiptName       = IntS7072.ReceiptName
/*[EXECUTABLE]*/                 IntPol7072.ReceiptAddr       = IntS7072.ReceiptAddr
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.AgentBrokerLicenseNumber = IntS7072.AgentBrokerLicenseNumber .
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntPol7072.ReferenceNumber      = IntS7072.ReferenceNumber
/*[EXECUTABLE]*/                 IntPol7072.EndorseEffectiveDt   = IntS7072.EndorseEffectiveDt
/*[EXECUTABLE]*/                 IntPol7072.EndorseRefNumber     = IntS7072.EndorseRefNumber
/*[EXECUTABLE]*/                 IntPol7072.EndorseFlag          = IntS7072.EndorseFlag /* Product Code ·Õè Premium */
/*[EXECUTABLE]*/                 IntPol7072.SystemRq             = IntS7072.SystemRq
/*[EXECUTABLE]*/                 IntPol7072.InsurerCode          = IntS7072.InsurerCode
/*[EXECUTABLE]*/                 IntPol7072.MethodCode           = IntS7072.MethodCode
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.Policy               = nv_PolicyV70
/*[EXECUTABLE]*/                 IntPol7072.Rencnt               = IntS7072.Rencnt
/*[EXECUTABLE]*/                 IntPol7072.Endcnt               = IntS7072.Endcnt
/*[EXECUTABLE]*/                 IntPol7072.Riskno               = IntS7072.Riskno
/*[EXECUTABLE]*/                 IntPol7072.Itemno               = IntS7072.Itemno
/*[EXECUTABLE]*/                 IntPol7072.SendByUser           = IntS7072.SendByUser
/*[EXECUTABLE]*/                 IntPol7072.SendDate             = IntS7072.SendDate
/*[EXECUTABLE]*/                 IntPol7072.SendTime             = IntS7072.SendTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.SystemErrorStatus1   = IntS7072.SystemErrorStatus1
/*[EXECUTABLE]*/                 IntPol7072.SystemErrorStatus2   = IntS7072.SystemErrorStatus2
/*[EXECUTABLE]*/                 IntPol7072.SystemErrorStatus3   = IntS7072.SystemErrorStatus3
/*[EXECUTABLE]*/                 IntPol7072.vehreg               = IntS7072.vehreg
/*[EXECUTABLE]*/                 IntPol7072.ReceiveNumber        = IntS7072.ReceiveNumber
/*[EXECUTABLE]*/                 IntPol7072.EndorseReceiveNumber = IntS7072.EndorseReceiveNumber
/*[EXECUTABLE]*/                 IntPol7072.accdat               = IntS7072.accdat
/*[EXECUTABLE]*/                 IntPol7072.comdat               = IntS7072.comdat
/*[EXECUTABLE]*/                 IntPol7072.expdat               = IntS7072.expdat
/*[EXECUTABLE]*/                 IntPol7072.ErrorMessage         = IntS7072.ErrorMessage
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.SumInsureAmt         = IntS7072.SumInsureAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.PolicyTypeCd              = IntS7072.PolicyTypeCd
/*[EXECUTABLE]*/                 IntPol7072.RateGroup                 = IntS7072.RateGroup
/*[EXECUTABLE]*/                 IntPol7072.GarageTypeCd              = IntS7072.GarageTypeCd
/*[EXECUTABLE]*/                 IntPol7072.GarageDesc                = IntS7072.GarageDesc
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.COLLAmtAccident           = IntS7072.COLLAmtAccident
/*[EXECUTABLE]*/                 IntPol7072.DeductibleCOLLAmtAccident = IntS7072.DeductibleCOLLAmtAccident 
/*[EXECUTABLE]*/                 IntPol7072.FTAmt                     = IntS7072.FTAmt
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.CMIPolicyTypeCd      = IntS7072.CMIPolicyTypeCd
/*[EXECUTABLE]*/                 IntPol7072.CMIVehTypeCd         = IntS7072.CMIVehTypeCd
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.CMIPolicyNumber      = nv_PolicyV72
/*[COMMENT]*/                    /*IntPol7072.CMIDocumentUID       = IntS7072.CMIDocumentUID /*nv_DocnoV72*/*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.CMIApplicationNumber = IntS7072.CMIApplicationNumber
/*[BLANK]*/                      
/*[COMMENT]*/                    /*IntPol7072.CMIBarCodeNumber     = IntS7072.CMIBarCodeNumber*/
/*[EXECUTABLE]*/                 IntPol7072.CMIEffectiveDt       = IntS7072.CMIEffectiveDt
/*[EXECUTABLE]*/                 IntPol7072.CMIExpirationDt      = IntS7072.CMIExpirationDt
/*[EXECUTABLE]*/                 IntPol7072.CMIAmtPerson         = IntS7072.CMIAmtPerson
/*[EXECUTABLE]*/                 IntPol7072.CMIAmtAccident       = IntS7072.CMIAmtAccident
/*[EXECUTABLE]*/                 IntPol7072.CMIWrittenAmt        = IntS7072.CMIWrittenAmt
/*[EXECUTABLE]*/                 IntPol7072.CMIRevenueStampAmt   = IntS7072.CMIRevenueStampAmt
/*[EXECUTABLE]*/                 IntPol7072.CMIVatAmt            = IntS7072.CMIVatAmt
/*[EXECUTABLE]*/                 IntPol7072.CMICurrentTermAmt    = IntS7072.CMICurrentTermAmt
/*[EXECUTABLE]*/                 IntPol7072.PromptText           = IntS7072.PromptText
/*[EXECUTABLE]*/                 IntPol7072.OptionValueDesc      = IntS7072.OptionValueDesc
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.RqUID                = nv_octets
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.keyRequestIndRq      = IntS7072.keyRequestIndRq
/*[EXECUTABLE]*/                 IntPol7072.TransactionDateRq    = IntS7072.TransactionDateRq
/*[EXECUTABLE]*/                 IntPol7072.TransactionTimeRq    = IntS7072.TransactionTimeRq
/*[EXECUTABLE]*/                 IntPol7072.MsgStatusCd          = IntS7072.MsgStatusCd
/*[EXECUTABLE]*/                 IntPol7072.BirthDate            = IntS7072.BirthDate
/*[EXECUTABLE]*/                 IntPol7072.BirthDate1           = IntS7072.BirthDate1
/*[EXECUTABLE]*/                 IntPol7072.BirthDate2           = IntS7072.BirthDate2
/*[EXECUTABLE]*/                 IntPol7072.InsUIDExpDate        = IntS7072.InsUIDExpDate
/*[EXECUTABLE]*/                 IntPol7072.InsUIDExpDate1       = IntS7072.InsUIDExpDate1
/*[EXECUTABLE]*/                 IntPol7072.InsUIDExpDate2       = IntS7072.InsUIDExpDate2
/*[EXECUTABLE]*/                 IntPol7072.CMIComDate           = IntS7072.CMIComDate
/*[EXECUTABLE]*/                 IntPol7072.CMIExpDate           = IntS7072.CMIExpDate
/*[BLANK]*/                      
/*[COMMENT]*/                    /*02/10/2015*/
/*[EXECUTABLE]*/                 IntPol7072.Statusflag = IntS7072.Statusflag /*Flag Ã¶·Ñ¹ã¨ à©¾ÒÐ»ÃÐàÀ· 1*/
/*[EXECUTABLE]*/                 IntPol7072.Finint     = IntS7072.Finint     /*Deler / VAT CODE*/
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.ResponseResult    = IntS7072.ResponseResult
/*[EXECUTABLE]*/                 IntPol7072.ResultStatus      = "SUCCESS"
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.ProcessStatus     = "X"
/*[EXECUTABLE]*/                 IntPol7072.ProcessByUser     = IntS7072.ProcessByUser
/*[EXECUTABLE]*/                 IntPol7072.ProcessDate       = IntS7072.ProcessDate
/*[EXECUTABLE]*/                 IntPol7072.ProcessTime       = IntS7072.ProcessTime
/*[EXECUTABLE]*/                 IntPol7072.TrnFromIntByUser  = IntS7072.TrnFromIntByUser
/*[EXECUTABLE]*/                 IntPol7072.TrnFromIntDate    = IntS7072.TrnFromIntDate
/*[EXECUTABLE]*/                 IntPol7072.TrnFromIntTime    = IntS7072.TrnFromIntTime
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.ConfirmBy         = IntS7072.ConfirmBy
/*[EXECUTABLE]*/                 IntPol7072.ByUserID          = IntS7072.ByUserID
/*[EXECUTABLE]*/                 IntPol7072.ChkVehicle        = IntS7072.ChkVehicle
/*[EXECUTABLE]*/                 IntPol7072.ChkVehBy          = IntS7072.ChkVehBy
/*[EXECUTABLE]*/                 IntPol7072.ChkVehDt          = IntS7072.ChkVehDt
/*[EXECUTABLE]*/                 IntPol7072.ChkVehTime        = IntS7072.ChkVehTime
/*[EXECUTABLE]*/                 IntPol7072.ChkVehSend        = IntS7072.ChkVehSend
/*[EXECUTABLE]*/                 IntPol7072.ChkVehMail        = IntS7072.ChkVehMail
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.ChkVehAssignBy    = IntS7072.ChkVehAssignBy
/*[EXECUTABLE]*/                 IntPol7072.ChkVehAssignDt    = IntS7072.ChkVehAssignDt
/*[EXECUTABLE]*/                 IntPol7072.ChkVehAssignTime  = IntS7072.ChkVehAssignTime
/*[EXECUTABLE]*/                 IntPol7072.ChkVehAssignSend  = IntS7072.ChkVehAssignSend
/*[EXECUTABLE]*/                 IntPol7072.ChkVehAssignMail  = IntS7072.ChkVehAssignMail
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.TransferToPremium = IntS7072.TransferToPremium .
/*[COMMENT]*/                    /* */
/*[COMMENT]*/                    /* --------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN PD_SAVEPD3. /*Save ÊèÇ¹·ÕèàËÅ×Í*/
/*[COMMENT]*/                    /* --------------------------------- */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF TRIM(IntPol7072.CMIDocumentUID) <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) < 7 THEN DO:
/*[BLANK]*/                        
/*[EXECUTABLE]*/                          IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 6 THEN IntPol7072.CMIDocumentUID = "0" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 5 THEN IntPol7072.CMIDocumentUID = "00" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 4 THEN IntPol7072.CMIDocumentUID = "000" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 3 THEN IntPol7072.CMIDocumentUID = "0000" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 2 THEN IntPol7072.CMIDocumentUID = "00000" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.CMIDocumentUID)) = 1 THEN IntPol7072.CMIDocumentUID = "000000" + TRIM(IntPol7072.CMIDocumentUID).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* */
/*[EXECUTABLE]*/                 IF TRIM(IntPol7072.DocumentUID) <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF LENGTH(TRIM(IntPol7072.DocumentUID)) < 7 THEN DO:
/*[BLANK]*/                        
/*[EXECUTABLE]*/                          IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 6 THEN IntPol7072.DocumentUID = "0" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 5 THEN IntPol7072.DocumentUID = "00" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 4 THEN IntPol7072.DocumentUID = "000" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 3 THEN IntPol7072.DocumentUID = "0000" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 2 THEN IntPol7072.DocumentUID = "00000" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                     ELSE IF LENGTH(TRIM(IntPol7072.DocumentUID)) = 1 THEN IntPol7072.DocumentUID = "000000" + TRIM(IntPol7072.DocumentUID).
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /* */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF nv_PolicyV70 <> "" THEN DO:
/*[EXECUTABLE]*/                   IF SUBSTR(nv_PolicyV70,1,1) = "Q"  THEN  IntPol7072.Rencnt = 0 . /*Add kridtiya i.§Ò¹ Q ÁÕrenew<>0 */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF IntPol7072.InsuranceBranchCd = "" THEN DO:
/*[COMMENT]*/                           /*D07057SK1234*/
/*[COMMENT]*/                           /*123456789012*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF    SUBSTR(nv_PolicyV70,1,1) = "D"
/*[EXECUTABLE]*/                        OR SUBSTR(nv_PolicyV70,1,1) = "Q" 
/*[EXECUTABLE]*/                        OR SUBSTR(nv_PolicyV70,1,1) = "R"
/*[EXECUTABLE]*/                     THEN 
/*[EXECUTABLE]*/                          IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,2,1).
/*[BLANK]*/                      
/*[COMMENT]*/                             /*347057C00005*/
/*[COMMENT]*/                             /*123456789012*/
/*[EXECUTABLE]*/                     ELSE IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,1,2).
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[EXECUTABLE]*/                 ELSE DO:
/*[EXECUTABLE]*/                   IF nv_PolicyV72 <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                     IF IntPol7072.InsuranceBranchCd = "" THEN DO:
/*[COMMENT]*/                             /*D07057SK1234*/
/*[COMMENT]*/                             /*123456789012*/
/*[EXECUTABLE]*/                       IF    SUBSTR(nv_PolicyV70,1,1) = "D" 
/*[EXECUTABLE]*/                          OR SUBSTR(nv_PolicyV70,1,1) = "Q"
/*[EXECUTABLE]*/                          OR SUBSTR(nv_PolicyV70,1,1) = "R"
/*[EXECUTABLE]*/                       THEN 
/*[EXECUTABLE]*/                            IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,2,1).
/*[BLANK]*/                        
/*[COMMENT]*/                               /*347057C00005*/
/*[COMMENT]*/                               /*123456789012*/
/*[EXECUTABLE]*/                       ELSE IntPol7072.InsuranceBranchCd = SUBSTR(nv_PolicyV70,1,2).
/*[BLANK]*/                        
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 END.
/*[COMMENT]*/                    /*First Loss ·Ø¹ 1 áÊ¹ ¡Ñº2 áÊ¹*/
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IF TRIM(IntPol7072.PolicyNumber) <> "" THEN DO:
/*[BLANK]*/                      
/*[EXECUTABLE]*/                   IF SUBSTR(IntPol7072.PolicyNumber,1,1) = "Q" /*AND nv_Poltyp = "V70" */
/*[EXECUTABLE]*/                      AND    IntPol7072.PolicyTypeCd      = "1" 
/*[EXECUTABLE]*/                      AND   (IntPol7072.RateGroup         = "110" OR IntPol7072.RateGroup = "320")
/*[EXECUTABLE]*/                   THEN DO:
/*[EXECUTABLE]*/                     IF     (DECIMAL(IntPol7072.SumInsureAmt) = 100000
/*[EXECUTABLE]*/                         OR  DECIMAL(IntPol7072.SumInsureAmt) = 200000)
/*[EXECUTABLE]*/                        AND (DECIMAL(IntPol7072.CurrentTermAmt) = 9899.64 
/*[EXECUTABLE]*/                         OR  DECIMAL(IntPol7072.CurrentTermAmt) = 12499.74
/*[EXECUTABLE]*/                         OR  DECIMAL(IntPol7072.CurrentTermAmt) = 12000.05)
/*[EXECUTABLE]*/                     THEN DO:
/*[EXECUTABLE]*/                       IntPol7072.EndorseFlag = "FIRST LOSS".  /* Product Code ·Õè Premium */
/*[BLANK]*/                        
/*[EXECUTABLE]*/                       IF TRIM(IntPol7072.ReceiptNumber) = ""  OR TRIM(IntPol7072.ReceiptNumber) = "0" OR
/*[EXECUTABLE]*/                          TRIM(IntPol7072.ReceiptNumber) = "O" 
/*[EXECUTABLE]*/                       THEN DO: /* Promotion ·Õè Premium */
/*[BLANK]*/                        
/*[EXECUTABLE]*/                         IF IntPol7072.RateGroup = "110" THEN IntPol7072.ReceiptNumber = "¤ØéÁ¤èÒ FL110".
/*[EXECUTABLE]*/                         IF IntPol7072.RateGroup = "320" THEN IntPol7072.ReceiptNumber = "¤ØéÁ¤èÒ FL320".
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[BLANK]*/                      
/*[COMMENT]*/                      /*442 ¤Ø³ÊÁÈÑ¡´Ôì ->à«ç¹·ÃÑÅ ÍÔ¹ªÑÇÃì*/
/*[EXECUTABLE]*/                   IF    (SUBSTR(IntPol7072.PolicyNumber,1,1) <> "Q" 
/*[EXECUTABLE]*/                     AND  SUBSTR(IntPol7072.PolicyNumber,1,1) <> "R")
/*[COMMENT]*/                            /**/
/*[EXECUTABLE]*/                     AND (IntPol7072.PolicyTypeCd = "2.2" OR IntPol7072.PolicyTypeCd = "3.2")
/*[COMMENT]*/                            /**/
/*[EXECUTABLE]*/                     AND (   IntPol7072.RateGroup = "110" OR IntPol7072.RateGroup = "210" 
/*[EXECUTABLE]*/                          OR IntPol7072.RateGroup = "320" )
/*[EXECUTABLE]*/                   THEN DO:
/*[EXECUTABLE]*/                     IF      DECIMAL(IntPol7072.SumInsureAmt) = 100000
/*[COMMENT]*/                                /**/
/*[EXECUTABLE]*/                        AND (DECIMAL(IntPol7072.CurrentTermAmt) = 7254.60 
/*[EXECUTABLE]*/                         OR  DECIMAL(IntPol7072.CurrentTermAmt) = 6608.32 )
/*[EXECUTABLE]*/                     THEN DO:
/*[EXECUTABLE]*/                       IntPol7072.EndorseFlag = "FIX".  /* Product Code ·Õè Premium */
/*[EXECUTABLE]*/                     END.
/*[EXECUTABLE]*/                   END.
/*[EXECUTABLE]*/                 END.
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 OUTPUT TO PD_SAVEPD2.TXT APPEND.
/*[EXECUTABLE]*/                 PUT "PD_SAVEPD2: 2. END: " FORMAT "x(25)"
/*[EXECUTABLE]*/                     " " TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                 SKIP.
/*[EXECUTABLE]*/                 OUTPUT CLOSE.
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEPD3 C-Win 
/*[EXECUTABLE]*/                 PROCEDURE PD_SAVEPD3 :
/*[COMMENT]*/                    /*------------------------------------------------------------------------------
/*[COMMENT]*/                      Purpose:     
/*[COMMENT]*/                      Parameters:  <none>
/*[COMMENT]*/                      Notes:       
/*[COMMENT]*/                    ------------------------------------------------------------------------------*/
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntPol7072.DriverNameCd     = IntS7072.DriverNameCd
/*[EXECUTABLE]*/                 IntPol7072.InsuredTitle1    = IntS7072.InsuredTitle1
/*[EXECUTABLE]*/                 IntPol7072.InsuredName1     = IntS7072.InsuredName1
/*[EXECUTABLE]*/                 IntPol7072.InsuredSurname1  = IntS7072.InsuredSurname1
/*[EXECUTABLE]*/                 IntPol7072.OccupationDesc1  = IntS7072.OccupationDesc1
/*[EXECUTABLE]*/                 IntPol7072.BirthDt1         = IntS7072.BirthDt1
/*[EXECUTABLE]*/                 IntPol7072.InsuredUniqueID1 = IntS7072.InsuredUniqueID1
/*[EXECUTABLE]*/                 IntPol7072.License1         = IntS7072.License1
/*[EXECUTABLE]*/                 IntPol7072.InsuredTitle2    = IntS7072.InsuredTitle2
/*[EXECUTABLE]*/                 IntPol7072.InsuredName2     = IntS7072.InsuredName2
/*[EXECUTABLE]*/                 IntPol7072.InsuredSurname2  = IntS7072.InsuredSurname2
/*[EXECUTABLE]*/                 IntPol7072.OccupationDesc2  = IntS7072.OccupationDesc2
/*[EXECUTABLE]*/                 IntPol7072.BirthDt2         = IntS7072.BirthDt2
/*[EXECUTABLE]*/                 IntPol7072.InsuredUniqueID2 = IntS7072.InsuredUniqueID2
/*[EXECUTABLE]*/                 IntPol7072.License2         = IntS7072.License2
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 IntPol7072.TPBIAmtPerson             = IntS7072.TPBIAmtPerson
/*[EXECUTABLE]*/                 IntPol7072.TPBIAmtAccident           = IntS7072.TPBIAmtAccident
/*[EXECUTABLE]*/                 IntPol7072.PDAmtAccident             = IntS7072.PDAmtAccident
/*[EXECUTABLE]*/                 IntPol7072.DeductiblePDAmtAccident   = IntS7072.DeductiblePDAmtAccident
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 IntPol7072.PerilsPADriverAmt              = IntS7072.PerilsPADriverAmt
/*[EXECUTABLE]*/                 IntPol7072.PerilsPANumPassengers          = IntS7072.PerilsPANumPassengers
/*[EXECUTABLE]*/                 IntPol7072.PerilsPAPassengerAmt           = IntS7072.PerilsPAPassengerAmt
/*[EXECUTABLE]*/                 IntPol7072.PerilsPATemporaryDriverAmt     = IntS7072.PerilsPATemporaryDriverAmt
/*[EXECUTABLE]*/                 IntPol7072.PerilsPANumTemporaryPassengers = IntS7072.PerilsPANumTemporaryPassengers
/*[EXECUTABLE]*/                 IntPol7072.PerilsPATemporaryPassengerAmt  = IntS7072.PerilsPATemporaryPassengerAmt
/*[EXECUTABLE]*/                 IntPol7072.PerilsMedicalTreatmentAmt      = IntS7072.PerilsMedicalTreatmentAmt
/*[EXECUTABLE]*/                 IntPol7072.PerilsBailBondInsuranceAmt     = IntS7072.PerilsBailBondInsuranceAmt
/*[EXECUTABLE]*/                 IntPol7072.PremiumCoverage13Amt           = IntS7072.PremiumCoverage13Amt
/*[EXECUTABLE]*/                 IntPol7072.DiscountForNamedDriver         = IntS7072.DiscountForNamedDriver
/*[EXECUTABLE]*/                 IntPol7072.DeductibleAmt        = IntS7072.DeductibleAmt
/*[EXECUTABLE]*/                 IntPol7072.FleetAmt             = IntS7072.FleetAmt
/*[EXECUTABLE]*/                 IntPol7072.GoodDriverIndPct     = IntS7072.GoodDriverIndPct
/*[EXECUTABLE]*/                 IntPol7072.GoodDriverIndAmt     = IntS7072.GoodDriverIndAmt
/*[EXECUTABLE]*/                 IntPol7072.TotalDiscountsAmt    = IntS7072.TotalDiscountsAmt
/*[EXECUTABLE]*/                 IntPol7072.SurchargeFactorAmt   = IntS7072.SurchargeFactorAmt
/*[EXECUTABLE]*/                 IntPol7072.PremiumCoverage2Amt  = IntS7072.PremiumCoverage2Amt
/*[EXECUTABLE]*/                 IntPol7072.OtherDiscountAmt     = IntS7072.OtherDiscountAmt
/*[EXECUTABLE]*/                 IntPol7072.ReceiptNumber        = IntS7072.ReceiptNumber   /*CampaignNumber/PromotionNumber*/
/*[EXECUTABLE]*/                 IntPol7072.ReceiptName2         = IntS7072.ReceiptName2
/*[EXECUTABLE]*/                 IntPol7072.ReceiptAddr2         = IntS7072.ReceiptAddr2
/*[EXECUTABLE]*/                 IntPol7072.Beneficiaries        = IntS7072.Beneficiaries
/*[EXECUTABLE]*/                 IntPol7072.PolicyAttachment     = IntS7072.PolicyAttachment
/*[EXECUTABLE]*/                 IntPol7072.VehicleUse           = IntS7072.VehicleUse .
/*[BLANK]*/                      
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 ASSIGN
/*[EXECUTABLE]*/                 IntPol7072.COUNTER_NO                = IntS7072.COUNTER_NO
/*[EXECUTABLE]*/                 IntPol7072.TERM_NO                   = IntS7072.TERM_NO
/*[EXECUTABLE]*/                 IntPol7072.SERVICE_RUN_NO            = IntS7072.SERVICE_RUN_NO
/*[EXECUTABLE]*/                 IntPol7072.RECORD_STATUS             = IntS7072.RECORD_STATUS
/*[EXECUTABLE]*/                 IntPol7072.CLIENT_SERVICE_RUNNO      = IntS7072.CLIENT_SERVICE_RUNNO
/*[EXECUTABLE]*/                 IntPol7072.ZONE                      = IntS7072.ZONE
/*[EXECUTABLE]*/                 IntPol7072.R_SERVICE_RUNN            = IntS7072.R_SERVICE_RUNN
/*[EXECUTABLE]*/                 IntPol7072.CANCEL_OPERATING          = IntS7072.CANCEL_OPERATING
/*[EXECUTABLE]*/                 IntPol7072.OPERATE_BY_STAFF          = IntS7072.OPERATE_BY_STAFF
/*[EXECUTABLE]*/                 IntPol7072.SYSTEM_DATE_TIME          = IntS7072.SYSTEM_DATE_TIME
/*[EXECUTABLE]*/                 IntPol7072.USERID_CS                 = IntS7072.USERID_CS
/*[EXECUTABLE]*/                 IntPol7072.PASSWORD_CS               = IntS7072.PASSWORD_CS
/*[EXECUTABLE]*/                 IntPol7072.SUCCESS                   = IntS7072.SUCCESS
/*[EXECUTABLE]*/                 IntPol7072.CODE                      = IntS7072.CODE
/*[EXECUTABLE]*/                 IntPol7072.DESC_CS                   = IntS7072.DESC_CS
/*[EXECUTABLE]*/                 IntPol7072.METHOD                    = IntS7072.METHOD
/*[EXECUTABLE]*/                 IntPol7072.TX_ID                     = IntS7072.TX_ID
/*[EXECUTABLE]*/                 IntPol7072.LOG_ID                    = IntS7072.LOG_ID
/*[EXECUTABLE]*/                 IntPol7072.VENDOR_ID                 = IntS7072.VENDOR_ID
/*[EXECUTABLE]*/                 IntPol7072.SERVICE_ID                = IntS7072.SERVICE_ID
/*[EXECUTABLE]*/                 IntPol7072.INSURER_PRODUCT_LINE_CODE = IntS7072.INSURER_PRODUCT_LINE_CODE
/*[EXECUTABLE]*/                 IntPol7072.PRODUCT_CODE              = IntS7072.PRODUCT_CODE
/*[EXECUTABLE]*/                 IntPol7072.PLAN_CODE                 = IntS7072.PLAN_CODE
/*[EXECUTABLE]*/                 IntPol7072.CATEGORY                  = IntS7072.CATEGORY
/*[EXECUTABLE]*/                 IntPol7072.BILLING_FREQ              = IntS7072.BILLING_FREQ
/*[EXECUTABLE]*/                 IntPol7072.NET_PREMIUM               = IntS7072.NET_PREMIUM
/*[EXECUTABLE]*/                 IntPol7072.GROSS_PREMIUM             = IntS7072.GROSS_PREMIUM
/*[EXECUTABLE]*/                 IntPol7072.SUM_INSURE                = IntS7072.SUM_INSURE
/*[EXECUTABLE]*/                 IntPol7072.PRINT_SLIP                = IntS7072.PRINT_SLIP
/*[EXECUTABLE]*/                 IntPol7072.POL_NO                    = IntS7072.POL_NO
/*[EXECUTABLE]*/                 IntPol7072.CERT_NO                   = IntS7072.CERT_NO
/*[EXECUTABLE]*/                 IntPol7072.OLD_POL_NO                = IntS7072.OLD_POL_NO
/*[EXECUTABLE]*/                 IntPol7072.NEW_RENEW                 = IntS7072.NEW_RENEW
/*[EXECUTABLE]*/                 IntPol7072.POL_YEAR_SEQ              = IntS7072.POL_YEAR_SEQ
/*[EXECUTABLE]*/                 IntPol7072.SALE_DATE                 = IntS7072.SALE_DATE
/*[EXECUTABLE]*/                 IntPol7072.EFFECTIVE_DATE            = IntS7072.EFFECTIVE_DATE
/*[EXECUTABLE]*/                 IntPol7072.END_DATE                  = IntS7072.END_DATE
/*[EXECUTABLE]*/                 IntPol7072.NID_NO                    = IntS7072.NID_NO
/*[EXECUTABLE]*/                 IntPol7072.CARD_TITLE                = IntS7072.CARD_TITLE
/*[EXECUTABLE]*/                 IntPol7072.CARD_NAME                 = IntS7072.CARD_NAME
/*[EXECUTABLE]*/                 IntPol7072.CARD_MNAME                = IntS7072.CARD_MNAME
/*[EXECUTABLE]*/                 IntPol7072.CARD_SNAME                = IntS7072.CARD_SNAME
/*[EXECUTABLE]*/                 IntPol7072.CARD_DOB                  = IntS7072.CARD_DOB
/*[EXECUTABLE]*/                 IntPol7072.CARD_GENDER               = IntS7072.CARD_GENDER
/*[EXECUTABLE]*/                 IntPol7072.CARD_ADDRESS              = IntS7072.CARD_ADDRESS
/*[EXECUTABLE]*/                 IntPol7072.CARD_SUB_DISTRICT         = IntS7072.CARD_SUB_DISTRICT
/*[EXECUTABLE]*/                 IntPol7072.CARD_DISTRICT             = IntS7072.CARD_DISTRICT
/*[EXECUTABLE]*/                 IntPol7072.CARD_PROVINCE             = IntS7072.CARD_PROVINCE
/*[EXECUTABLE]*/                 IntPol7072.CARD_ISSUE_DATE           = IntS7072.CARD_ISSUE_DATE
/*[EXECUTABLE]*/                 IntPol7072.CARD_EXPIRED_DATE         = IntS7072.CARD_EXPIRED_DATE
/*[EXECUTABLE]*/                 IntPol7072.TEL_NO                    = IntS7072.TEL_NO
/*[EXECUTABLE]*/                 IntPol7072.CURRENT_ADDRESS           = IntS7072.CURRENT_ADDRESS
/*[EXECUTABLE]*/                 IntPol7072.CURRENT_SUB_DISTRICT      = IntS7072.CURRENT_SUB_DISTRICT
/*[EXECUTABLE]*/                 IntPol7072.CURRENT_DISTRICT          = IntS7072.CURRENT_DISTRICT
/*[EXECUTABLE]*/                 IntPol7072.CURRENT_PROVINCE          = IntS7072.CURRENT_PROVINCE
/*[EXECUTABLE]*/                 IntPol7072.PAYCODE                   = IntS7072.PAYCODE
/*[EXECUTABLE]*/                 IntPol7072.CHASSIS_NO                = IntS7072.CHASSIS_NO
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_REG_DATE          = IntS7072.VEHICLE_REG_DATE
/*[EXECUTABLE]*/                 IntPol7072.CAR_REG_NO                = IntS7072.CAR_REG_NO
/*[EXECUTABLE]*/                 IntPol7072.CAR_REG_PROVINCE          = IntS7072.CAR_REG_PROVINCE
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_TYPE_CODE         = IntS7072.VEHICLE_TYPE_CODE
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_BODY_TYPE         = IntS7072.VEHICLE_BODY_TYPE
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_MAKE              = IntS7072.VEHICLE_MAKE
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_MODEL             = IntS7072.VEHICLE_MODEL
/*[EXECUTABLE]*/                 IntPol7072.VEHICLE_USE_CODE          = IntS7072.VEHICLE_USE_CODE
/*[EXECUTABLE]*/                 IntPol7072.ENGINE_CC                 = IntS7072.ENGINE_CC
/*[EXECUTABLE]*/                 IntPol7072.USE_AREA                  = IntS7072.USE_AREA
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_TITLE_1            = IntS7072.DRIVER_TITLE_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_NAME_1             = IntS7072.DRIVER_NAME_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_SURNAME_1          = IntS7072.DRIVER_SURNAME_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_GENDER_1           = IntS7072.DRIVER_GENDER_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_AGE_1              = IntS7072.DRIVER_AGE_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_LICENSE_1          = IntS7072.DRIVER_LICENSE_1
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_TITLE_2            = IntS7072.DRIVER_TITLE_2
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_NMAE_2             = IntS7072.DRIVER_NMAE_2
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_SURNAME_2          = IntS7072.DRIVER_SURNAME_2
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_GENDER_2           = IntS7072.DRIVER_GENDER_2
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_AGE_2              = IntS7072.DRIVER_AGE_2
/*[EXECUTABLE]*/                 IntPol7072.DRIVER_LICENSE_2          = IntS7072.DRIVER_LICENSE_2
/*[EXECUTABLE]*/                 IntPol7072.COMP_BARCODE              = IntS7072.COMP_BARCODE
/*[EXECUTABLE]*/                 IntPol7072.RECEIVE_NO                = IntS7072.RECEIVE_NO
/*[EXECUTABLE]*/                 IntPol7072.INVOICE_NO                = IntS7072.INVOICE_NO
/*[EXECUTABLE]*/                 IntPol7072.STAMP_RATE                = IntS7072.STAMP_RATE
/*[EXECUTABLE]*/                 IntPol7072.VAT                       = IntS7072.VAT .
/*[COMMENT]*/                    /* ------ */
/*[COMMENT]*/                    /**/
/*[EXECUTABLE]*/                 END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PD_SAVEQGwCtx C-Win 
/*[UNCALLED]*/                   PROCEDURE PD_SAVEQGwCtx :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /**
/*[UNCALLED]*/                   DEFINE INPUT PARAMETER nv_QRecIntPol7072  AS RECID NO-UNDO.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   DEFINE VARIABLE nv_NewInputKey            AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   /* ------------------------------------------------------------ */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     FIND IntPol7072 WHERE RECID(IntPol7072) = nv_QRecIntPol7072
/*[UNCALLED]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     /*
/*[UNCALLED]*/                     RUN wctx\WCtxGW100 (INPUT IntPol7072.PolicyNumber
/*[UNCALLED]*/                                        ,INPUT IntPol7072.CompanyCode).
/*[UNCALLED]*/                     */
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     nv_NewInputKey = STRING(YEAR(TODAY),"9999")
/*[UNCALLED]*/                                    + STRING(MONTH(TODAY),"99")
/*[UNCALLED]*/                                    + STRING(DAY(TODAY),"99")
/*[UNCALLED]*/                                    + SUBSTR(STRING(DATETIME(TODAY, MTIME)),12,12).
/*[UNCALLED]*/                     nv_NewInputKey = REPLACE(nv_NewInputKey,":","").
/*[UNCALLED]*/                     nv_NewInputKey = REPLACE(nv_NewInputKey,".","").
/*[UNCALLED]*/                     /**/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     CREATE IntQPolicy.
/*[UNCALLED]*/                     ASSIGN
/*[UNCALLED]*/                     IntQPolicy.SystemRq        = "SGwCtx"
/*[UNCALLED]*/                     IntQPolicy.keyRequestIndRq = nv_NewInputKey
/*[UNCALLED]*/                     IntQPolicy.ProcessStatus   = ""
/*[UNCALLED]*/                     IntQPolicy.Policy          = IntPol7072.PolicyNumber
/*[UNCALLED]*/                     IntQPolicy.Rencnt          = IntPol7072.Rencnt
/*[UNCALLED]*/                     IntQPolicy.Endcnt          = IntPol7072.Endcnt
/*[UNCALLED]*/                     IntQPolicy.PolicyRec       = 0                       /*RECID(uwm100)*/
/*[UNCALLED]*/                     IntQPolicy.ProcessByUser   = IntPol7072.CompanyCode
/*[UNCALLED]*/                     IntQPolicy.ProcessDate     = TODAY
/*[UNCALLED]*/                     IntQPolicy.ProcessTime     = STRING(TIME,"HH:MM:SS") + "." + SUBSTR(STRING(MTIME,">>>>99999999"),10,3) 
/*[UNCALLED]*/                     IntQPolicy.PolicyTypeCd    = IntPol7072.PolicyTypeCd
/*[UNCALLED]*/                     IntQPolicy.RateGroup       = IntPol7072.RateGroup
/*[UNCALLED]*/                     IntQPolicy.Releas          = YES .                  /*uwm100.releas*/
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     OUTPUT TO SavetoIntQPolicy.TXT APPEND.
/*[UNCALLED]*/                     PUT TODAY FORMAT "99/99/9999" 
/*[UNCALLED]*/                     " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[UNCALLED]*/                     " " IntPol7072.PolicyNumber FORMAT "x(16)"
/*[UNCALLED]*/                         IntPol7072.CompanyCode  FORMAT "x(10)" SKIP.
/*[UNCALLED]*/                     OUTPUT CLOSE.
/*[UNCALLED]*/                   **/
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_FileAttach2 C-Win 
/*[UNCALLED]*/                   PROCEDURE proc_FileAttach2 :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:       
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   /*create by Kridtiya i.    Lockton */
/*[UNCALLED]*/                   DEFINE INPUT        PARAMETER nv_CompanyCode            AS CHARACTER NO-UNDO.              
/*[UNCALLED]*/                   DEFINE INPUT        PARAMETER nv_PolicyType             AS CHARACTER NO-UNDO. /*v70,v72*/  
/*[UNCALLED]*/                   DEFINE INPUT        PARAMETER nv_CMIPolicyTypeCd        AS CHARACTER NO-UNDO.  
/*[UNCALLED]*/                   DEFINE INPUT        PARAMETER nv_RECIDIntPol7072        AS RECID NO-UNDO.
/*[UNCALLED]*/                   DEFINE INPUT-OUTPUT PARAMETER nv_NameCompCd             AS CHARACTER NO-UNDO.              
/*[UNCALLED]*/                   DEFINE INPUT-OUTPUT PARAMETER nv_PrgName                AS CHARACTER NO-UNDO.              
/*[UNCALLED]*/                   DEFINE INPUT-OUTPUT PARAMETER nv_PrmPrg                 AS CHARACTER NO-UNDO.  
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   DEFINE              VARIABLE  nv_SAVEmsgerror          AS CHARACTER NO-UNDO.
/*[UNCALLED]*/                   /**/
/*[UNCALLED]*/                   /**/
/*[UNCALLED]*/                   /*
/*[UNCALLED]*/                   nv_PolicyType  = "V70".
/*[UNCALLED]*/                   nv_CompanyCode = "210".
/*[UNCALLED]*/                   nv_CMIPolicyTypeCd = "3".
/*[UNCALLED]*/                   */
/*[UNCALLED]*/                     /* ---------------------------------------------------- */
/*[UNCALLED]*/                     /* ProgramPrint ¾Ãº. form PDF àÅ¢ DOCNO ¢Í§ºÃÔÉÑ··ÕèàºÔ¡ */
/*[UNCALLED]*/                   IF IntPol7072.SERVICE_ID = "online" THEN nv_CompanyCode = TRIM(nv_CompanyCode) + "cer".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   ASSIGN
/*[UNCALLED]*/                     nv_NameCompCd = "" nv_PrmPrg = "" nv_PrgName = "".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                     FIND FIRST FNameAttach WHERE
/*[UNCALLED]*/                                FNameAttach.CompanyCode  = nv_CompanyCode
/*[UNCALLED]*/                            AND FNameAttach.PolicyTypeCd = nv_PolicyType      /*"V70"*/
/*[UNCALLED]*/                            AND FNameAttach.CoverTypeCd  = nv_CMIPolicyTypeCd /*¾Ãº ËÃ×Í "T", 3, 3.1*/
/*[UNCALLED]*/                            AND FNameAttach.EffDate     <= TODAY
/*[UNCALLED]*/                            AND FNameAttach.SelectNumber = 2
/*[UNCALLED]*/                     NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                     IF AVAILABLE FNameAttach THEN DO:
/*[UNCALLED]*/                         ASSIGN 
/*[UNCALLED]*/                             nv_NameCompCd = FNameAttach.CompanyCode
/*[UNCALLED]*/                             nv_PrgName    = FNameAttach.PrgName
/*[UNCALLED]*/                             nv_PrmPrg     = FNameAttach.PrmPrg.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     /*IF nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr709".
/*[UNCALLED]*/                     ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /* Wctxr709 */*/
/*[UNCALLED]*/                     IF   nv_PrgName = "" THEN nv_PrgName = "Wctx/Wctxr702_RP".
/*[UNCALLED]*/                                          ELSE nv_PrgName = "Wctx/" + nv_PrgName.  /* Wctxr709 */
/*[UNCALLED]*/                   FIND IntPol7072 WHERE RECID(IntPol7072) = nv_RECIDIntPol7072 NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                   IF NOT AVAILABLE IntPol7072 THEN RETURN.
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   RUN VALUE(nv_PrgName)
/*[UNCALLED]*/                       (IntPol7072.CompanyCode         /*nv_BrokerCompany*/   /*nv_BrokerCompany*/
/*[UNCALLED]*/                       ,IntPol7072.CMIPolicyNumber                        
/*[UNCALLED]*/                       ,IntPol7072.Rencnt                                 
/*[UNCALLED]*/                       ,IntPol7072.Endcnt                                 
/*[UNCALLED]*/                       ,IntPol7072.CMIDocumentUID                         
/*[UNCALLED]*/                       ,IntPol7072.RqUID               /*nv_code  keyRequestIndRq*/     /*nv_code  keyRequestIndRq*/
/*[UNCALLED]*/                       ,""                             /*n_user   */                          /*n_user   */
/*[UNCALLED]*/                       ,""                             /*n_passwd */                          
/*[UNCALLED]*/                       ,nv_PrmPrg                      /*Name Report*/                        /*n_passwd */
/*[UNCALLED]*/                       ,OUTPUT nv_SAVEmsgerror). 
/*[UNCALLED]*/                   IF IntPol7072.SERVICE_ID = "online" THEN nv_NameCompCd = TRIM(IntPol7072.CompanyCode) + "cer".
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /* _UIB-CODE-BLOCK-END */
/*[EXECUTABLE]*/                 &ANALYZE-RESUME
/*[BLANK]*/                      
