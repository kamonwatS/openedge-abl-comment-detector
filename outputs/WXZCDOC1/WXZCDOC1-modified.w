/*[COMMENT]*/          /*========================================================================*/
/*[COMMENT]*/          /* Program Name : WxzCDoc1.P                                              */
/*[COMMENT]*/          /*                Program Running Document                                */
/*[COMMENT]*/          /* CREATE  By   : Tantawan Ch.   ASSIGN: A61-0019 (Date 19/12/2019)       */
/*[COMMENT]*/          /*========================================================================*/
/*[COMMENT]*/          /* Modify By    : Tantawan Ch.   ASSIGN: A62-0279  DATE : 29/01/2020      */
/*[COMMENT]*/          /*              : »Ô´¡ÒÃ GEN Qeue SEND Mail                               */
/*[COMMENT]*/          /*========================================================================*/
/*[COMMENT]*/          /* Modify By    :  Songkran P.  ASSIGN: A62-0279  DATE : 01/02/2020      */
/*[COMMENT]*/          /*              : à¾ÔèÁ loop repeat Ç¹ËÒàÅ¢¨¹¡ÇèÒ¨Ðä´éàÅ¢àÍ¡ÊÒÃ           */
/*[COMMENT]*/          /*========================================================================*/
/*[BLANK]*/            
/*[COMMENT]*/          /*1*/  DEF INPUT   PARAMETER  gv_choice      AS INT.  /* 1 , 2 , 3 ...     */
/*[COMMENT]*/          /*2*/  DEF INPUT   PARAMETER  gv_DocYr       AS INT.  /* 2019              */
/*[COMMENT]*/          /*3*/  DEF INPUT   PARAMETER  gv_DocType     AS CHAR. /* M = Receipt , S = Invoice , T = Sticker , C = Certificate ...*/
/*[COMMENT]*/          /*4*/  DEF INPUT   PARAMETER  gv_bc          AS CHAR. /* "D0" , "ALL" , "STY" , "B300222"     */
/*[EXECUTABLE]*/       /*5*/  DEF INPUT   PARAMETER  gv_usrid       AS CHAR.
/*[EXECUTABLE]*/       DEF INPUT   PARAMETER  gv_auto        AS LOG.
/*[EXECUTABLE]*/       /*6*/  DEF OUTPUT  PARAMETER  gv_docrun1     AS CHAR.
/*[EXECUTABLE]*/       /*7*/  DEF OUTPUT  PARAMETER  gv_docrun2     AS CHAR.
/*[EXECUTABLE]*/       /*8*/  DEF OUTPUT  PARAMETER  gv_docrun3     AS CHAR.
/*[EXECUTABLE]*/       /*9*/  DEF OUTPUT  PARAMETER  gv_error       AS CHAR.
/*[BLANK]*/            
/*[EXECUTABLE]*/       DEF VAR nv_prog      AS CHAR INIT "WXZCDOC1".
/*[EXECUTABLE]*/       DEF VAR nv_YrPref    AS CHAR.
/*[EXECUTABLE]*/       DEF VAR nv_docdup    AS LOG.
/*[EXECUTABLE]*/       DEF VAR nv_first     AS LOG.
/*[EXECUTABLE]*/       DEF VAR nv_catecd    AS CHAR.
/*[BLANK]*/            
/*[EXECUTABLE]*/       DEF VAR nv_Quota     AS DECI.
/*[EXECUTABLE]*/       DEF VAR nv_docgrp    AS CHAR.
/*[BLANK]*/            
/*[EXECUTABLE]*/       DEF BUFFER bDocRunn   FOR DocRunn.
/*[BLANK]*/            
/*[EXECUTABLE]*/       ASSIGN
/*[EXECUTABLE]*/          gv_docrun1 = ""
/*[EXECUTABLE]*/          gv_docrun2 = ""
/*[EXECUTABLE]*/          gv_docrun3 = ""
/*[EXECUTABLE]*/          gv_error   = ""
/*[EXECUTABLE]*/          nv_catecd  = "".
/*[BLANK]*/            
/*[EXECUTABLE]*/       nv_YrPref = SUBSTRING(STRING(gv_DocYr + 543),3,2).
/*[BLANK]*/            
/*[EXECUTABLE]*/       FIND FIRST DocMst USE-INDEX DocMst01 
/*[EXECUTABLE]*/           WHERE DocMst.b_c = gv_bc NO-LOCK NO-ERROR.
/*[EXECUTABLE]*/       IF NOT AVAIL DocMst THEN DO :
/*[EXECUTABLE]*/           gv_error = "Not found Document Master Parameter Setup for Code :" + gv_bc .
/*[EXECUTABLE]*/           RETURN.
/*[EXECUTABLE]*/       END.
/*[EXECUTABLE]*/       ELSE 
/*[EXECUTABLE]*/           ASSIGN
/*[EXECUTABLE]*/           nv_catecd = DocMst.CateCd
/*[EXECUTABLE]*/           nv_docgrp = DocMst.Docgrp.
/*[BLANK]*/            
/*[EXECUTABLE]*/       IF gv_DocType = "S" OR gv_DocType = "B" OR gv_DocType = "D" OR gv_DocType = "C" THEN 
/*[EXECUTABLE]*/           ASSIGN
/*[COMMENT]*/              /*nv_catecd = "ALL"*/
/*[EXECUTABLE]*/           nv_docgrp = "8".
/*[BLANK]*/            
/*[EXECUTABLE]*/       OUTPUT TO RunDoc-Log.TXT APPEND.
/*[EXECUTABLE]*/       PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." SUBSTR(STRING(MTIME,">>>>99999999"),10,3)
/*[EXECUTABLE]*/                   "PARAM: "
/*[EXECUTABLE]*/               "gv_choice: " string(gv_choice)  FORMAT "X(5)"
/*[EXECUTABLE]*/              "gv_Doctype: " gv_Doctype         FORMAT "X(5)"
/*[EXECUTABLE]*/        "       nv_docgrp: " nv_docgrp          FORMAT "X(16)"
/*[EXECUTABLE]*/               "nv_catecd: " nv_catecd          FORMAT "X(16)" SKIP.
/*[EXECUTABLE]*/       OUTPUT CLOSE.
/*[BLANK]*/            
/*[EXECUTABLE]*/       IF gv_error = "" THEN DO:
/*[COMMENT]*/              /* Single Type */
/*[EXECUTABLE]*/           IF gv_choice = 1 OR gv_choice = 2 OR gv_choice = 3 THEN DO:
/*[BLANK]*/            
/*[EXECUTABLE]*/               IF gv_DocType = "T" THEN DO:  /* Type 'T' = Sticker */
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RUN  RunSticker (INPUT  gv_DocType,
/*[COMMENT]*/                                       OUTPUT gv_docrun1,
/*[COMMENT]*/                                       OUTPUT gv_error).
/*[COMMENT]*/                      */
/*[EXECUTABLE]*/                   RUN  RunSticker2 (INPUT  gv_DocType,
/*[EXECUTABLE]*/                                     OUTPUT gv_docrun1,
/*[EXECUTABLE]*/                                     OUTPUT gv_error).
/*[EXECUTABLE]*/               END.  
/*[EXECUTABLE]*/               ELSE DO: /* Type <> "T" */
/*[BLANK]*/                        
/*[EXECUTABLE]*/                   RUN  RunDocument (INPUT  gv_DocType,
/*[EXECUTABLE]*/                                     OUTPUT gv_docrun1,
/*[EXECUTABLE]*/                                     OUTPUT gv_error).
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/           END. 
/*[BLANK]*/                
/*[EXECUTABLE]*/           IF gv_error = "" THEN DO:
/*[BLANK]*/                
/*[EXECUTABLE]*/               IF gv_choice = 2 OR gv_choice = 3 THEN DO: /* Receipt + Invoice */
/*[BLANK]*/            
/*[EXECUTABLE]*/                   gv_docrun2 = gv_docrun1. /* Receipt & Invoice ãªéàÅ¢à´ÕÂÇ¡Ñ¹ */
/*[BLANK]*/            
/*[EXECUTABLE]*/               END.
/*[BLANK]*/            
/*[EXECUTABLE]*/               IF gv_choice = 3 THEN DO: /**/
/*[COMMENT]*/                      /*
/*[COMMENT]*/                      RUN  RunSticker (INPUT  "T",
/*[COMMENT]*/                                       OUTPUT gv_docrun3, /* Êè§¤èÒ Sticker ¡ÅÑº */
/*[COMMENT]*/                                       OUTPUT gv_error).
/*[COMMENT]*/                      */
/*[EXECUTABLE]*/                   RUN  RunSticker2 (INPUT  gv_DocType,
/*[EXECUTABLE]*/                                     OUTPUT gv_docrun3,
/*[EXECUTABLE]*/                                     OUTPUT gv_error).
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/           END.
/*[BLANK]*/            
/*[EXECUTABLE]*/       END. /* gv_error = "" */
/*[EXECUTABLE]*/       RELEASE DocRunn.
/*[EXECUTABLE]*/       RELEASE bDocRunn.
/*[BLANK]*/            
/*[EXECUTABLE]*/       RETURN.
/*[BLANK]*/            
/*[COMMENT]*/          /********************************************************************/
/*[EXECUTABLE]*/       PROCEDURE RunDocument:
/*[BLANK]*/            
/*[EXECUTABLE]*/        DEF INPUT  PARAMETER pv_DocType  AS CHAR.
/*[EXECUTABLE]*/        DEF OUTPUT PARAMETER pv_docrun1  AS CHAR.
/*[EXECUTABLE]*/        DEF OUTPUT PARAMETER pv_error    AS CHAR.
/*[BLANK]*/            
/*[EXECUTABLE]*/        DEF VAR nv_lineCins  AS INT  NO-UNDO.
/*[EXECUTABLE]*/        DEF VAR nv_next      AS DECI INIT 0.
/*[EXECUTABLE]*/        DEF VAR nv_dup       AS LOG.
/*[BLANK]*/            
/*[EXECUTABLE]*/        pv_error = "".
/*[EXECUTABLE]*/        loop_RunDoc:
/*[EXECUTABLE]*/        REPEAT:
/*[EXECUTABLE]*/           FIND FIRST DocRunn USE-INDEX DocRunn01
/*[EXECUTABLE]*/                       WHERE DocRunn.DocYr   = gv_DocYr
/*[EXECUTABLE]*/                       AND   DocRunn.DocTyp  = pv_DocType
/*[EXECUTABLE]*/                       AND   DocRunn.DocGrp  = nv_docgrp
/*[EXECUTABLE]*/                       AND   DocRunn.CateCd  = nv_CateCd 
/*[EXECUTABLE]*/                       AND   DECI(DocRunn.docpref + DocRunn.docnoto) >= DocRunn.DocNext EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/           IF NOT AVAIL DocRunn THEN DO:
/*[EXECUTABLE]*/               IF LOCKED DocRunn THEN DO:
/*[EXECUTABLE]*/                   nv_lineCins  = 0.
/*[EXECUTABLE]*/                   DO WHILE nv_lineCins  <= 100 :
/*[EXECUTABLE]*/                     nv_lineCins  = nv_lineCins  + 1.
/*[EXECUTABLE]*/                   END.   
/*[COMMENT]*/                      /*pv_error = "Record is Locked".*/
/*[EXECUTABLE]*/                   NEXT loop_RunDoc.
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/               ELSE DO:
/*[BLANK]*/            
/*[EXECUTABLE]*/                   IF gv_auto THEN DO:  /* ¡Ã³ÕµéÍ§¡ÒÃ auto setup àÁ×èÍ¢Öé¹»ÕãËÁè */
/*[COMMENT]*/                          /* ËÒ Doc Running »Õ»Ñ¨¨ØºÑ¹ */
/*[EXECUTABLE]*/                       FIND LAST DocRunn USE-INDEX DocRunn01
/*[EXECUTABLE]*/                           WHERE DocRunn.DocYr   = gv_DocYr
/*[EXECUTABLE]*/                           AND   DocRunn.DocTyp  = pv_DocType
/*[EXECUTABLE]*/                           AND   DocRunn.DocGrp  = nv_docgrp
/*[EXECUTABLE]*/                           AND   DocRunn.CateCd  = nv_CateCd NO-LOCK NO-ERROR NO-WAIT.
/*[EXECUTABLE]*/                       IF AVAIL DocRunn THEN DO:  /* ¶éÒÁÕáÊ´§ÁÕ¡ÒÃ Set áÅéÇáµè Running àµçÁ */
/*[EXECUTABLE]*/                           pv_error = "(1)Document running is full, Please contract Helpdesk...!".
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                       ELSE DO:  /* ¶éÒäÁè¾º  ¶×ÍÇèÒà»ç¹ New Set ¢Í§»ÕãËÁè */
/*[BLANK]*/            
/*[COMMENT]*/                              /* Find Previous data Lot áÃ¡ */
/*[EXECUTABLE]*/                           FIND FIRST bDocRunn USE-INDEX DocRunn01
/*[EXECUTABLE]*/                               WHERE bDocRunn.DocTyp  = pv_DocType
/*[EXECUTABLE]*/                               AND   bDocRunn.DocGrp  = nv_docgrp
/*[EXECUTABLE]*/                               AND   bDocRunn.CateCd  = nv_CateCd  NO-LOCK NO-ERROR.
/*[EXECUTABLE]*/                           IF NOT AVAIL bDocRunn THEN DO:
/*[EXECUTABLE]*/                               pv_error = "(1)Not found document running parameter setup Year " + STRING(gv_DocYr,"9999") + " Type:" + pv_DocType + " Category:" + nv_CateCd + ", Please contract Helpdesk...!".
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                           ELSE DO:
/*[EXECUTABLE]*/                               nv_dup = NO.
/*[COMMENT]*/                                  /* Check Dup ¡èÍ¹¨Ð Auto Create */
/*[EXECUTABLE]*/                               RUN Chk_Dup (INPUT  bDocRunn.doctyp, 
/*[EXECUTABLE]*/                                                   bDocRunn.docnofr,
/*[EXECUTABLE]*/                                                   bDocRunn.docnoto,
/*[EXECUTABLE]*/                                            OUTPUT nv_dup).
/*[BLANK]*/                
/*[EXECUTABLE]*/                               IF nv_dup = NO THEN DO:  /* äÁè«éÓ Create ä´é */
/*[COMMENT]*/                                      /* Auto Create */
/*[EXECUTABLE]*/                                   CREATE DocRunn.
/*[EXECUTABLE]*/                                   ASSIGN 
/*[EXECUTABLE]*/                                       DocRunn.docyr    =  gv_DocYr     
/*[EXECUTABLE]*/                                       DocRunn.doctyp   =  bDocRunn.doctyp     
/*[EXECUTABLE]*/                                       DocRunn.docgrp   =  bDocRunn.docgrp     
/*[EXECUTABLE]*/                                       DocRunn.b_c      =  bDocRunn.b_c        
/*[EXECUTABLE]*/                                       DocRunn.acno     =  bDocRunn.acno       
/*[EXECUTABLE]*/                                       DocRunn.catecd   =  bDocRunn.catecd 
/*[EXECUTABLE]*/                                       DocRunn.doclotno =  1
/*[BLANK]*/                                            
/*[EXECUTABLE]*/                                       DocRunn.docpref  =  bDocRunn.docgrp + nv_YrPref    
/*[EXECUTABLE]*/                                       DocRunn.docnofr  =  bDocRunn.docnofr    
/*[EXECUTABLE]*/                                       DocRunn.docnoto  =  bDocRunn.docnoto    
/*[EXECUTABLE]*/                                       DocRunn.docnext  =  DECI(bDocRunn.docgrp + nv_YrPref + bDocRunn.docnofr)  /* ¤èÒàÃÔèÁµé¹à·èÒ¡Ñº Document From */
/*[BLANK]*/                                            
/*[EXECUTABLE]*/                                       DocRunn.usrid    =  gv_usrid
/*[EXECUTABLE]*/                                       DocRunn.entdat   =  TODAY     
/*[EXECUTABLE]*/                                       DocRunn.enttim   =  STRING(TIME,"HH:MM:SS")
/*[EXECUTABLE]*/                                       DocRunn.prog     =  nv_prog
/*[EXECUTABLE]*/                                       DocRunn.revdat   =  TODAY
/*[EXECUTABLE]*/                                       DocRunn.remark   =  "Auto Create".
/*[BLANK]*/                            
/*[EXECUTABLE]*/                                   IF DEC(DocRunn.DocNext) > DEC(DocRunn.DocPref + DocRunn.DocnoTo) THEN DO:
/*[EXECUTABLE]*/                                       DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[EXECUTABLE]*/                                       pv_error = "(2)Document running is full, Please contract Helpdesk...!".
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                                   ELSE DO:
/*[EXECUTABLE]*/                                       pv_docrun1 = STRING(DocRunn.DocNext,"9999999999").
/*[EXECUTABLE]*/                                       nv_next    = 0.
/*[EXECUTABLE]*/                                       nv_next    = DocRunn.DocNext.
/*[BLANK]*/                            
/*[EXECUTABLE]*/                                       ASSIGN
/*[EXECUTABLE]*/                                         DocRunn.DocNext = nv_next  + 1
/*[EXECUTABLE]*/                                         DocRunn.usrid   = gv_usrid
/*[EXECUTABLE]*/                                         DocRunn.revdat  = TODAY.
/*[EXECUTABLE]*/                                       IF nv_next >= DECI(DocRunn.DocPref + STRING(DocRunn.DocnoTo,"9999999")) THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[BLANK]*/            
/*[COMMENT]*/                                          /*-- A62-0279 -- »Ô´ Queue  ---
/*[COMMENT]*/                                          nv_Quota = DECI(DocRunn.docpref + DocRunn.docnoto) - DECI(DocRunn.docNext).
/*[COMMENT]*/                                          RUN PD_GenQMail (INPUT nv_Quota ,
/*[COMMENT]*/                                                                 DocRunn.docyr ,
/*[COMMENT]*/                                                                 DocRunn.doctyp,
/*[COMMENT]*/                                                                 DocRunn.CateCd,
/*[COMMENT]*/                                                                 DocRunn.doclotno).
/*[COMMENT]*/                                          -- A62-0279 -- »Ô´ Queue  ---*/                       
/*[EXECUTABLE]*/                                   END.
/*[EXECUTABLE]*/                               END.
/*[EXECUTABLE]*/                               ELSE pv_error = "(1)New Document running is duplicate, Please contract Helpdesk for manual setup Running...!".
/*[EXECUTABLE]*/                           END.
/*[EXECUTABLE]*/                       END.
/*[EXECUTABLE]*/                   END. /* IF gv_auto = YES */
/*[EXECUTABLE]*/                   ELSE pv_error = "(2)Not found document running parameter setup Year " + STRING(gv_DocYr,"9999") + " Type:" + pv_DocType + " Category:" + nv_CateCd + ", Please contract Helpdesk...!".
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/           END.
/*[EXECUTABLE]*/           ELSE DO: /* AVAIL */
/*[COMMENT]*/                  /* ¶éÒÁÒ¡¡ÇèÒáÊ´§ÇèÒ running àµçÁ */
/*[EXECUTABLE]*/               IF DEC(DocRunn.DocNext) > DEC(DocRunn.DocPref + DocRunn.DocnoTo) THEN DO:
/*[EXECUTABLE]*/                   DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[EXECUTABLE]*/                   pv_error = "(3)Document running is full, Please contract Helpdesk...!".
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/               ELSE DO:
/*[EXECUTABLE]*/                   pv_docrun1 = STRING(DocRunn.DocNext,"9999999999").
/*[EXECUTABLE]*/                   nv_next   =  DocRunn.DocNext.
/*[BLANK]*/                        
/*[EXECUTABLE]*/                   ASSIGN
/*[EXECUTABLE]*/                     DocRunn.DocNext = nv_next  + 1
/*[EXECUTABLE]*/                     DocRunn.usrid   = gv_usrid
/*[EXECUTABLE]*/                     DocRunn.revdat  = TODAY.
/*[EXECUTABLE]*/                   IF nv_next >= DECI(DocRunn.DocPref + STRING(DocRunn.DocnoTo,"9999999")) THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[BLANK]*/            
/*[COMMENT]*/                      /*-- A62-0279 -- »Ô´ Queue  ---
/*[COMMENT]*/                      nv_Quota = DECI(DocRunn.docpref + DocRunn.docnoto) - DECI(DocRunn.docNext).
/*[COMMENT]*/                      RUN PD_GenQMail (INPUT nv_Quota ,
/*[COMMENT]*/                                             DocRunn.docyr ,
/*[COMMENT]*/                                             DocRunn.doctyp,
/*[COMMENT]*/                                             DocRunn.CateCd,
/*[COMMENT]*/                                             DocRunn.doclotno).
/*[COMMENT]*/                      -- A62-0279 -- »Ô´ Queue  ---*/
/*[BLANK]*/                        
/*[EXECUTABLE]*/               END.
/*[EXECUTABLE]*/           END.
/*[EXECUTABLE]*/           LEAVE Loop_RunDoc.
/*[EXECUTABLE]*/        END. /* REPEAT */ 
/*[EXECUTABLE]*/        RELEASE DocRunn.
/*[EXECUTABLE]*/        RELEASE bDocRunn.
/*[BLANK]*/             
/*[EXECUTABLE]*/       END.
/*[COMMENT]*/          /********************************************************************/
/*[BLANK]*/            
/*[COMMENT]*/          /********************************************************************/
/*[COMMENT]*/          /* Running Sticker áºº Running áÂ¡µÒÁ»Õ */
/*[UNCALLED]*/         PROCEDURE RunSticker:
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF INPUT  PARAMETER pv_DocType  AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_docrun2   AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_error    AS CHAR.
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF VAR nv_lineCins  AS INT  NO-UNDO.
/*[UNCALLED]*/             DEF VAR nv_StkNext   AS CHAR INIT "".
/*[UNCALLED]*/             DEF VAR nv_dup       AS LOG.
/*[UNCALLED]*/         
/*[UNCALLED]*/             pv_error = "".
/*[UNCALLED]*/             Loop_RunStk:
/*[UNCALLED]*/             REPEAT:
/*[UNCALLED]*/                 FIND FIRST DocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                     WHERE DocRunn.DocYr   = gv_DocYr
/*[UNCALLED]*/                     AND   DocRunn.DocTyp  = pv_DocType
/*[UNCALLED]*/                     AND   DocRunn.CateCd  = nv_CateCd 
/*[UNCALLED]*/                     AND   DocRunn.StkPref + STRING(DocRunn.Stknoto)  >= DocRunn.Stknext EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                 IF NOT AVAIL DocRunn THEN DO:
/*[UNCALLED]*/                     IF LOCKED DocRunn THEN DO:
/*[UNCALLED]*/                         nv_lineCins  = 0.
/*[UNCALLED]*/                         DO WHILE nv_lineCins  <= 100 :
/*[UNCALLED]*/                           nv_lineCins  = nv_lineCins  + 1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         /*pv_error = "Record is Locked".*/
/*[UNCALLED]*/                         NEXT Loop_RunStk.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                         IF gv_auto THEN DO:  /* ¡Ã³ÕµéÍ§¡ÒÃ auto setup àÁ×èÍ¢Öé¹»ÕãËÁè */
/*[UNCALLED]*/                             /* ËÒ Doc Running »Õ»Ñ¨¨ØºÑ¹ */
/*[UNCALLED]*/                             FIND LAST DocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                                 WHERE DocRunn.DocYr   = gv_DocYr
/*[UNCALLED]*/                                 AND   DocRunn.DocTyp  = pv_DocType
/*[UNCALLED]*/                                 AND   DocRunn.CateCd  = nv_CateCd NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                             IF AVAIL DocRunn THEN DO:  /* ¶éÒÁÕáÊ´§ÁÕ¡ÒÃ Set áÅéÇáµè Running àµçÁ */
/*[UNCALLED]*/                                 pv_error = "(1)Sticker running is full, Please contract Helpdesk...!".
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                             ELSE DO:  /* ¶éÒäÁè¾º  ¶×ÍÇèÒà»ç¹ New Set ¢Í§»ÕãËÁè */
/*[UNCALLED]*/                 
/*[UNCALLED]*/                                 /* Find Previous data Lot áÃ¡ */
/*[UNCALLED]*/                                 FIND FIRST bDocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                                     WHERE bDocRunn.DocTyp  = pv_DocType
/*[UNCALLED]*/                                     AND   bDocRunn.CateCd  = nv_CateCd NO-LOCK NO-ERROR.
/*[UNCALLED]*/                                 IF NOT AVAIL bDocRunn THEN DO:
/*[UNCALLED]*/                                     pv_error = "(1)Not found sticker running parameter setup Type:" + pv_DocType + ", Please contract Helpdesk...!".
/*[UNCALLED]*/                                 END.
/*[UNCALLED]*/                                 ELSE DO:
/*[UNCALLED]*/                                     nv_dup = NO.
/*[UNCALLED]*/                                     /* Check Dup ¡èÍ¹¨Ð Auto Create */
/*[UNCALLED]*/                                     RUN Chk_Dup (INPUT  bDocRunn.doctyp, 
/*[UNCALLED]*/                                                         bDocRunn.Stknofr,
/*[UNCALLED]*/                                                         bDocRunn.Stknoto,
/*[UNCALLED]*/                                                  OUTPUT nv_dup).
/*[UNCALLED]*/                     
/*[UNCALLED]*/                                     IF nv_dup = NO THEN DO:  /* äÁè«éÓ Create ä´é */
/*[UNCALLED]*/                     
/*[UNCALLED]*/                                         /* Auto Create */
/*[UNCALLED]*/                                         CREATE DocRunn.
/*[UNCALLED]*/                                         ASSIGN 
/*[UNCALLED]*/                                             DocRunn.docyr    =  gv_DocYr     
/*[UNCALLED]*/                                             DocRunn.doctyp   =  bDocRunn.doctyp     
/*[UNCALLED]*/                                             DocRunn.docgrp   =  bDocRunn.docgrp     
/*[UNCALLED]*/                                             DocRunn.b_c      =  bDocRunn.b_c        
/*[UNCALLED]*/                                             DocRunn.acno     =  bDocRunn.acno       
/*[UNCALLED]*/                                             DocRunn.catecd   =  bDocRunn.catecd 
/*[UNCALLED]*/                                             DocRunn.doclotno =  1
/*[UNCALLED]*/                 
/*[UNCALLED]*/                                             DocRunn.StkPref  =  SUBSTRING(bDocRunn.StkPref,1,2) + bDocRunn.docgrp + nv_YrPref
/*[UNCALLED]*/                                             DocRunn.Stknofr  =  bDocRunn.Stknofr    
/*[UNCALLED]*/                                             DocRunn.Stknoto  =  bDocRunn.Stknoto    
/*[UNCALLED]*/                                             DocRunn.Stknext  =  SUBSTRING(bDocRunn.StkPref,1,2) + bDocRunn.docgrp + nv_YrPref + bDocRunn.Stknofr /* ¤èÒàÃÔèÁµé¹à·èÒ¡Ñº Document From */
/*[UNCALLED]*/                         
/*[UNCALLED]*/                                             DocRunn.usrid    =  gv_usrid
/*[UNCALLED]*/                                             DocRunn.entdat   =  TODAY     
/*[UNCALLED]*/                                             DocRunn.enttim   =  STRING(TIME,"HH:MM:SS")
/*[UNCALLED]*/                                             DocRunn.prog     =  nv_prog
/*[UNCALLED]*/                                             DocRunn.revdat   =  TODAY
/*[UNCALLED]*/                                             DocRunn.remark   =  "Auto Create".
/*[UNCALLED]*/                         
/*[UNCALLED]*/                                         /* ¶éÒÁÒ¡¡ÇèÒáÊ´§ÇèÒ running àµçÁ */
/*[UNCALLED]*/                                         IF DEC(DocRunn.StkNext) > DEC(DocRunn.StkPref + DocRunn.StknoTo) THEN DO:
/*[UNCALLED]*/                                             DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                                             pv_error = "(2)Sticker running is full, Please contract Helpdesk...!".
/*[UNCALLED]*/                                         END.
/*[UNCALLED]*/                                         ELSE DO:
/*[UNCALLED]*/                                             /* Sticker no ÂÑ§äÁèà¤Â¶Ù¡´Ö§ä»ãªé§Ò¹ */
/*[UNCALLED]*/                                             IF STRING(DocRunn.StkPref + DocRunn.StknoFr,"9999999999999") = DocRunn.StkNext THEN DO:
/*[UNCALLED]*/                                                 nv_first = YES.
/*[UNCALLED]*/                                                 /* Run & Check Mod àÅ¢áÃ¡à¾×èÍ Update Å§  Sticker Next */
/*[UNCALLED]*/                                                 RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                                                    nv_first ,
/*[UNCALLED]*/                                                             OUTPUT nv_StkNext).
/*[UNCALLED]*/                         
/*[UNCALLED]*/                                                 /* Update  Sticker Next àºÍÃìáÃ¡ */
/*[UNCALLED]*/                                                 ASSIGN
/*[UNCALLED]*/                                                   DocRunn.StkNext = STRING(nv_StkNext,"9999999999999")
/*[UNCALLED]*/                                                   DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                                                   DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                                                 IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                                                 /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                                                 nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                                                 RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                                        DocRunn.docyr ,
/*[UNCALLED]*/                                                                        DocRunn.doctyp,
/*[UNCALLED]*/                                                                        DocRunn.CateCd,
/*[UNCALLED]*/                                                                        DocRunn.doclotno).
/*[UNCALLED]*/                                                 -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                                             END.
/*[UNCALLED]*/                         
/*[UNCALLED]*/                                             nv_first = NO.
/*[UNCALLED]*/                                             /* */
/*[UNCALLED]*/                                             pv_docrun2 = STRING(DocRunn.StkNext,"9999999999999").
/*[UNCALLED]*/                                             /* Run & Check MOD */
/*[UNCALLED]*/                                             RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                                                nv_first ,
/*[UNCALLED]*/                                                         OUTPUT nv_StkNext).
/*[UNCALLED]*/                                             /* Update  Sticker Next */
/*[UNCALLED]*/                                             ASSIGN
/*[UNCALLED]*/                                               DocRunn.StkNext = STRING(nv_StkNext,"9999999999999")
/*[UNCALLED]*/                                               DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                                               DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                                             IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                                             /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                                             nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                                             RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                                    DocRunn.docyr ,
/*[UNCALLED]*/                                                                    DocRunn.doctyp,
/*[UNCALLED]*/                                                                    DocRunn.CateCd,
/*[UNCALLED]*/                                                                    DocRunn.doclotno).
/*[UNCALLED]*/                                             -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                                         END.
/*[UNCALLED]*/                                     END.
/*[UNCALLED]*/                                     ELSE pv_error = "New Sticker running is duplicate, Please contract Helpdesk for manual setup Running...!".
/*[UNCALLED]*/                                 END.
/*[UNCALLED]*/                             END.
/*[UNCALLED]*/                         END. /* IF gv_auto = YES */
/*[UNCALLED]*/                         ELSE pv_error = "(2)Not found sticker running parameter setup Type:" + pv_DocType + ", Please contract Helpdesk...!".
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE DO:  /* AVAIL */
/*[UNCALLED]*/                     /* ¶éÒÁÒ¡¡ÇèÒáÊ´§ÇèÒ running àµçÁ */
/*[UNCALLED]*/                     IF DEC(DocRunn.StkNext) > DEC(DocRunn.StkPref + DocRunn.StknoTo) THEN DO:
/*[UNCALLED]*/                         DocRunn.remark = "FULL - " + DocRunn.remark.
/*[UNCALLED]*/                         pv_error = "(3)Sticker running is full, Please contract Helpdesk...!".
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                         /* Sticker no ÂÑ§äÁèà¤Â¶Ù¡´Ö§ä»ãªé§Ò¹ */
/*[UNCALLED]*/                         IF STRING(DocRunn.StkPref + DocRunn.StknoFr,"9999999999999") = DocRunn.StkNext THEN DO:
/*[UNCALLED]*/                             nv_first = YES.
/*[UNCALLED]*/                             /* Run & Check Mod àÅ¢áÃ¡à¾×èÍ Update Å§  Sticker Next */
/*[UNCALLED]*/                             RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                                nv_first ,
/*[UNCALLED]*/                                         OUTPUT nv_StkNext).
/*[UNCALLED]*/                 
/*[UNCALLED]*/                             /* Update  Sticker Next àºÍÃìáÃ¡ */
/*[UNCALLED]*/                             ASSIGN
/*[UNCALLED]*/                               DocRunn.StkNext = STRING(DocRunn.Stkpref + nv_StkNext,"9999999999999")
/*[UNCALLED]*/                               DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                               DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                             IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                             /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                             nv_Quota = DECI(DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                             RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                    DocRunn.docyr ,
/*[UNCALLED]*/                                                    DocRunn.doctyp,
/*[UNCALLED]*/                                                    DocRunn.CateCd,
/*[UNCALLED]*/                                                    DocRunn.doclotno).
/*[UNCALLED]*/                             -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                 
/*[UNCALLED]*/                         nv_first = NO.
/*[UNCALLED]*/                         /* */
/*[UNCALLED]*/                         pv_docrun2 = STRING(DocRunn.StkNext,"9999999999999").
/*[UNCALLED]*/                         /* Run & Check MOD */
/*[UNCALLED]*/                         RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                            nv_first ,
/*[UNCALLED]*/                                     OUTPUT nv_StkNext).
/*[UNCALLED]*/                         /* Update  Sticker Next */
/*[UNCALLED]*/                         ASSIGN
/*[UNCALLED]*/                           DocRunn.StkNext = STRING(nv_StkNext,"9999999999999")
/*[UNCALLED]*/                           DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                           DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                         IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                         /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                         nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                         RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                DocRunn.docyr ,
/*[UNCALLED]*/                                                DocRunn.doctyp,
/*[UNCALLED]*/                                                DocRunn.CateCd,
/*[UNCALLED]*/                                                DocRunn.doclotno).
/*[UNCALLED]*/                         -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/             
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 LEAVE Loop_RunStk.
/*[UNCALLED]*/             END. /* REPEAT */
/*[UNCALLED]*/         
/*[UNCALLED]*/             RELEASE DocRunn.
/*[UNCALLED]*/             RELEASE bDocRunn.
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         
/*[UNCALLED]*/         /*---- 2020-02-07 -----
/*[UNCALLED]*/         /* Running Sticker áºº Running µèÍà¹×èÍ§¨¹¡ÇèÒ¨ÐàµçÁ  
/*[UNCALLED]*/           äÁèÁÕ¡ÒÃ Auto Set Runnning                        */
/*[UNCALLED]*/         PROCEDURE RunSticker2:
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF INPUT  PARAMETER pv_DocType  AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_docrun2   AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_error    AS CHAR.
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF VAR nv_lineCins  AS INT  NO-UNDO.
/*[UNCALLED]*/             DEF VAR nv_StkNext   AS CHAR INIT "".
/*[UNCALLED]*/             DEF VAR nv_dup       AS LOG.
/*[UNCALLED]*/         
/*[UNCALLED]*/             pv_error = "".
/*[UNCALLED]*/             REPEAT:
/*[UNCALLED]*/         
/*[UNCALLED]*/                 FIND FIRST DocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                     WHERE DocRunn.DocYr = 2020
/*[UNCALLED]*/                     AND   DocRunn.DocTyp  = "T"
/*[UNCALLED]*/                     AND   DocRunn.CateCd  = nv_CateCd 
/*[UNCALLED]*/                     AND   DocRunn.StkPref + STRING(DocRunn.Stknoto)  >= DocRunn.Stknext EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                 IF NOT AVAIL DocRunn THEN DO:
/*[UNCALLED]*/                     IF LOCKED DocRunn THEN DO:
/*[UNCALLED]*/                         nv_lineCins  = 0.
/*[UNCALLED]*/                         DO WHILE nv_lineCins  <= 100 :
/*[UNCALLED]*/                           nv_lineCins  = nv_lineCins  + 1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/         
/*[UNCALLED]*/         /* OUTPUT TO Sticker-lock.txt APPEND.                            */
/*[UNCALLED]*/         /* PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." */
/*[UNCALLED]*/         /*   " nv_CateCd : " nv_CateCd FORMAT "X(15)"                    */
/*[UNCALLED]*/         /*   " pv_docrun2 : " pv_docrun2 FORMAT "X(15)" SKIP.            */
/*[UNCALLED]*/         /* OUTPUT CLOSE.                                                 */
/*[UNCALLED]*/         
/*[UNCALLED]*/                         /*pv_error = "Record is Locked".*/
/*[UNCALLED]*/                         NEXT.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE pv_error = "(2)Not found sticker running parameter setup, Please contract Helpdesk...!".
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE DO:  /* AVAIL */
/*[UNCALLED]*/             
/*[UNCALLED]*/                     /* ¶éÒÁÒ¡¡ÇèÒáÊ´§ÇèÒ running àµçÁ */
/*[UNCALLED]*/                     IF DEC(DocRunn.StkNext) > DEC(DocRunn.StkPref + DocRunn.StknoTo) THEN DO:
/*[UNCALLED]*/                         DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                         pv_error = "(4)Sticker running is full, Please contract Helpdesk...!".
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                         /* Sticker no ÂÑ§äÁèà¤Â¶Ù¡´Ö§ä»ãªé§Ò¹ */
/*[UNCALLED]*/         
/*[UNCALLED]*/                         IF STRING(DocRunn.StkPref + DocRunn.StknoFr,"9999999999999") = DocRunn.StkNext THEN DO:
/*[UNCALLED]*/                             nv_first = YES.
/*[UNCALLED]*/                             /* Run & Check Mod àÅ¢áÃ¡à¾×èÍ Update Å§  Sticker Next */
/*[UNCALLED]*/                             RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                                nv_first ,
/*[UNCALLED]*/                                         OUTPUT nv_StkNext).
/*[UNCALLED]*/                 
/*[UNCALLED]*/                             /* Update  Sticker Next àºÍÃìáÃ¡ */
/*[UNCALLED]*/                             DocRunn.StkNext = STRING(nv_StkNext,"9999999999999").
/*[UNCALLED]*/                             /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                             nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                             RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                    DocRunn.docyr ,
/*[UNCALLED]*/                                                    DocRunn.doctyp,
/*[UNCALLED]*/                                                    DocRunn.CateCd,
/*[UNCALLED]*/                                                    DocRunn.doclotno).
/*[UNCALLED]*/                             -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                 
/*[UNCALLED]*/                         nv_first = NO.
/*[UNCALLED]*/                         /* */
/*[UNCALLED]*/                         pv_docrun2 = STRING(DocRunn.StkNext,"9999999999999").
/*[UNCALLED]*/         
/*[UNCALLED]*/         /* OUTPUT TO Sticker.txt APPEND.                                 */
/*[UNCALLED]*/         /* PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." */
/*[UNCALLED]*/         /*   " DocRunn.DocTyp : " DocRunn.DocTyp FORMAT "X(5)"           */
/*[UNCALLED]*/         /*   " nv_CateCd : " nv_CateCd FORMAT "X(15)"                    */
/*[UNCALLED]*/         /*   " pv_docrun2 : " pv_docrun2 FORMAT "X(15)" SKIP.            */
/*[UNCALLED]*/         /* OUTPUT CLOSE.                                                 */
/*[UNCALLED]*/         
/*[UNCALLED]*/         
/*[UNCALLED]*/                         /* Run & Check MOD */
/*[UNCALLED]*/                         RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                            nv_first ,
/*[UNCALLED]*/                                     OUTPUT nv_StkNext).
/*[UNCALLED]*/                         /* Update  Sticker Next */
/*[UNCALLED]*/                         DocRunn.StkNext = STRING(nv_StkNext,"9999999999999").
/*[UNCALLED]*/                         IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                         /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                         nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                         RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                DocRunn.docyr ,
/*[UNCALLED]*/                                                DocRunn.doctyp,
/*[UNCALLED]*/                                                DocRunn.CateCd,
/*[UNCALLED]*/                                                DocRunn.doclotno).
/*[UNCALLED]*/                         -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 LEAVE.
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             RELEASE DocRunn.
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         ---- 2020-02-07 -----*/
/*[UNCALLED]*/         
/*[UNCALLED]*/         /* Running Sticker áºº Running µèÍà¹×èÍ§¨¹¡ÇèÒ¨ÐàµçÁ  
/*[UNCALLED]*/           äÁèÁÕ¡ÒÃ Auto Set Runnning                        */
/*[UNCALLED]*/         PROCEDURE RunSticker2:
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF INPUT  PARAMETER pv_DocType  AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_docrun2   AS CHAR.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER pv_error    AS CHAR.
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF VAR nv_lineCins  AS INT  NO-UNDO.
/*[UNCALLED]*/             DEF VAR nv_StkNext   AS CHAR INIT "".
/*[UNCALLED]*/             DEF VAR nv_dup       AS LOG.
/*[UNCALLED]*/         
/*[UNCALLED]*/             pv_error = "".
/*[UNCALLED]*/             Loop_RunStk:
/*[UNCALLED]*/             REPEAT:
/*[UNCALLED]*/                 FIND FIRST DocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                     WHERE Docrunn.DocYr   >= 2020
/*[UNCALLED]*/                     AND   DocRunn.DocTyp  = pv_DocType
/*[UNCALLED]*/                     AND   DocRunn.CateCd  = nv_CateCd 
/*[UNCALLED]*/                     AND   DocRunn.StkPref + STRING(DocRunn.Stknoto)  >= DocRunn.Stknext EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/                 IF NOT AVAIL DocRunn THEN DO:
/*[UNCALLED]*/                     IF LOCKED DocRunn THEN DO:
/*[UNCALLED]*/                         nv_lineCins  = 0.
/*[UNCALLED]*/                         DO WHILE nv_lineCins  <= 100 :
/*[UNCALLED]*/                           nv_lineCins  = nv_lineCins  + 1.
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                         /*pv_error = "Record is Locked".*/
/*[UNCALLED]*/                         NEXT Loop_RunStk.
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE pv_error = "(2)Not found sticker running parameter setup, Please contract Helpdesk...!".
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE DO:  /* AVAIL */
/*[UNCALLED]*/             
/*[UNCALLED]*/                     /* ¶éÒÁÒ¡¡ÇèÒáÊ´§ÇèÒ running àµçÁ */
/*[UNCALLED]*/                     IF DEC(DocRunn.StkNext) > DEC(DocRunn.StkPref + DocRunn.StknoTo) THEN DO:
/*[UNCALLED]*/                         DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                         pv_error = "(4)Sticker running is full, Please contract Helpdesk...!".
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                     ELSE DO:
/*[UNCALLED]*/                         /* Sticker no ÂÑ§äÁèà¤Â¶Ù¡´Ö§ä»ãªé§Ò¹ */
/*[UNCALLED]*/         
/*[UNCALLED]*/                         IF STRING(DocRunn.StkPref + DocRunn.StknoFr,"9999999999999") = DocRunn.StkNext THEN DO:
/*[UNCALLED]*/                             nv_first = YES.
/*[UNCALLED]*/                             /* Run & Check Mod àÅ¢áÃ¡à¾×èÍ Update Å§  Sticker Next */
/*[UNCALLED]*/                             RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                                nv_first ,
/*[UNCALLED]*/                                         OUTPUT nv_StkNext).
/*[UNCALLED]*/                 
/*[UNCALLED]*/                             /* Update  Sticker Next àºÍÃìáÃ¡ */
/*[UNCALLED]*/                             ASSIGN
/*[UNCALLED]*/                               DocRunn.StkNext = STRING(nv_StkNext,"9999999999999")
/*[UNCALLED]*/                               DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                               DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                             IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                             /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                             nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                             RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                    DocRunn.docyr ,
/*[UNCALLED]*/                                                    DocRunn.doctyp,
/*[UNCALLED]*/                                                    DocRunn.CateCd,
/*[UNCALLED]*/                                                    DocRunn.doclotno).
/*[UNCALLED]*/                             -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                         END.
/*[UNCALLED]*/                 
/*[UNCALLED]*/                         nv_first = NO.
/*[UNCALLED]*/                         /* */
/*[UNCALLED]*/                         pv_docrun2 = STRING(DocRunn.StkNext,"9999999999999").
/*[UNCALLED]*/                         /* Run & Check MOD */
/*[UNCALLED]*/                         RUN StkMod (INPUT  DocRunn.StkNext,  /* XX16200000001  */
/*[UNCALLED]*/                                            nv_first ,
/*[UNCALLED]*/                                     OUTPUT nv_StkNext).
/*[UNCALLED]*/                         /* Update  Sticker Next */
/*[UNCALLED]*/                         ASSIGN
/*[UNCALLED]*/                           DocRunn.StkNext = STRING(nv_StkNext,"9999999999999")
/*[UNCALLED]*/                           DocRunn.usrid   = gv_usrid
/*[UNCALLED]*/                           DocRunn.revdat  = TODAY.
/*[UNCALLED]*/                         IF nv_StkNext >= STRING(DocRunn.StkPref + DocRunn.StknoTo,"9999999999999") THEN DocRunn.remark = "FULL - " + DocRunn.remark. /* áÊ´§Ê¶Ò¹ÐÇèÒ Full */
/*[UNCALLED]*/                         /*-- A62-0279 -- »Ô´ Queue  ---
/*[UNCALLED]*/                         nv_Quota = DECI(DocRunn.Stkpref + DocRunn.Stknoto) - DECI(DocRunn.StkNext).
/*[UNCALLED]*/                         RUN PD_GenQMail (INPUT nv_Quota ,
/*[UNCALLED]*/                                                DocRunn.docyr ,
/*[UNCALLED]*/                                                DocRunn.doctyp,
/*[UNCALLED]*/                                                DocRunn.CateCd,
/*[UNCALLED]*/                                                DocRunn.doclotno).
/*[UNCALLED]*/                         -- A62-0279 -- »Ô´ Queue  ---*/
/*[UNCALLED]*/                     END.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 LEAVE Loop_RunStk.
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             RELEASE DocRunn.
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         
/*[UNCALLED]*/         /********************************************************************/
/*[UNCALLED]*/         PROCEDURE StkMod:
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF INPUT  PARAMETER  pv_stknext    AS CHAR.
/*[UNCALLED]*/             DEF INPUT  PARAMETER  pv_first      AS LOG.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER  pv_StkNo      AS CHAR.
/*[UNCALLED]*/             
/*[UNCALLED]*/             DEF VAR chr_sticker AS CHAR .
/*[UNCALLED]*/             DEF VAR nv_sck_no   AS CHAR.
/*[UNCALLED]*/             
/*[UNCALLED]*/             DEF VAR nv_leng12   AS INT.
/*[UNCALLED]*/             DEF VAR nv_cnt2     AS INT.
/*[UNCALLED]*/             
/*[UNCALLED]*/             DEF VAR Chk_mod1    AS DECI.
/*[UNCALLED]*/             DEF VAR Chk_mod2    AS DECI.
/*[UNCALLED]*/             DEF VAR nv_modulo   AS DECI.
/*[UNCALLED]*/             
/*[UNCALLED]*/             DEF VAR nv_sticker  AS CHAR FORMAT "9999999999999".
/*[UNCALLED]*/         
/*[UNCALLED]*/             /*===================== CheckMod =====================*/
/*[UNCALLED]*/             ASSIGN
/*[UNCALLED]*/                CHR_sticker = pv_stknext
/*[UNCALLED]*/         
/*[UNCALLED]*/                nv_sck_no   = pv_stknext
/*[UNCALLED]*/                nv_leng12   = LENGTH(nv_sck_no) - 1 .             /* 12 */
/*[UNCALLED]*/                nv_sck_no   = SUBSTRING(nv_sck_no,1,nv_leng12).
/*[UNCALLED]*/         
/*[UNCALLED]*/             IF pv_first THEN DO:   /* ãªé¤èÒ Sticker no µÑÇáÃ¡ä» MOD */ 
/*[UNCALLED]*/                 IF SUBSTRING(CHR_sticker,1,1) = "0" THEN chr_sticker  = "0" + STRING(DECI(nv_sck_no)).
/*[UNCALLED]*/                                                     ELSE chr_sticker  =       STRING(DECI(nv_sck_no)).
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             ELSE DO:
/*[UNCALLED]*/                 IF SUBSTRING(CHR_sticker,1,1) = "0" THEN chr_sticker  = "0" + STRING(DECI(nv_sck_no) + 1).
/*[UNCALLED]*/                                                     ELSE chr_sticker  =       STRING(DECI(nv_sck_no) + 1).
/*[UNCALLED]*/             END.
/*[UNCALLED]*/         
/*[UNCALLED]*/             nv_cnt2      = LENGTH(chr_sticker).     /* 12 */
/*[UNCALLED]*/         
/*[UNCALLED]*/             IF SUBSTRING (CHR_sticker,1,1) = "0"  THEN DO:
/*[UNCALLED]*/         
/*[UNCALLED]*/                 Chk_mod1 = DEC(SUBSTRING(chr_sticker,1,nv_cnt2)).            
/*[UNCALLED]*/                    
/*[UNCALLED]*/                 IF nv_cnt2 = 14 THEN DO:
/*[UNCALLED]*/                    Chk_mod2 = DEC(SUBSTRING(STRING( Chk_mod1 / 7,"99999999999999.999"  ),1,nv_cnt2)) * 7.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE IF nv_cnt2 = 12 THEN DO:
/*[UNCALLED]*/                    Chk_mod2 = DEC(SUBSTRING(STRING( Chk_mod1 / 7,"999999999999.999"  ),1,nv_cnt2)) * 7.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE IF nv_cnt2 = 10 THEN DO:
/*[UNCALLED]*/                    Chk_mod2 = DEC(SUBSTRING(STRING( Chk_mod1 / 7,"9999999999.999"  ),1,nv_cnt2)) * 7.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 ELSE IF nv_cnt2 = 8 THEN DO:
/*[UNCALLED]*/                    Chk_mod2 = DEC(SUBSTRING(STRING( Chk_mod1 / 7,"99999999.999"  ),1,nv_cnt2)) * 7.
/*[UNCALLED]*/                 END.
/*[UNCALLED]*/                 
/*[UNCALLED]*/                 nv_modulo = Chk_mod1 - Chk_mod2.  
/*[UNCALLED]*/                 nv_sticker = "0" + SUBSTRING(STRING(Chk_mod1),1,nv_cnt2) + STRING(nv_modulo). 
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             ELSE DO:
/*[UNCALLED]*/                 
/*[UNCALLED]*/                 Chk_mod1 = DEC(SUBSTRING(chr_sticker,1,nv_cnt2)).      
/*[UNCALLED]*/                 Chk_mod2 = DEC(SUBSTRING(STRING( Chk_mod1 / 7),1,nv_cnt2)) * 7.
/*[UNCALLED]*/                 
/*[UNCALLED]*/                 nv_modulo  = Chk_mod1 - Chk_mod2.         
/*[UNCALLED]*/                 nv_sticker = SUBSTRING(STRING(Chk_mod1),1,nv_cnt2) + STRING(nv_modulo).
/*[UNCALLED]*/                 /*---------------------------------------------------*/
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             
/*[UNCALLED]*/             pv_StkNo = nv_sticker. /* return Sticker no ¡ÅÑº */
/*[UNCALLED]*/         
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         /********************************************************************/
/*[UNCALLED]*/         
/*[UNCALLED]*/         
/*[UNCALLED]*/         PROCEDURE Chk_Dup.
/*[UNCALLED]*/         
/*[UNCALLED]*/             DEF INPUT  PARAMETER   chk_doctype  AS CHAR.
/*[UNCALLED]*/             DEF INPUT  PARAMETER   chk_docnofr  AS DECI.
/*[UNCALLED]*/             DEF INPUT  PARAMETER   chk_docnoto  AS DECI.
/*[UNCALLED]*/             DEF OUTPUT PARAMETER   chk_dup      AS LOG.
/*[UNCALLED]*/         
/*[UNCALLED]*/             chk_dup = NO.
/*[UNCALLED]*/             IF gv_DocType <> "T" THEN DO:
/*[UNCALLED]*/             
/*[UNCALLED]*/                 FIND FIRST  DocRunn USE-INDEX DocRunn02
/*[UNCALLED]*/                   WHERE    (DocRunn.Doctyp  = chk_doctype
/*[UNCALLED]*/                   AND (DECI(DocRunn.DocPref + STRING(DocRunn.DocnoFr,"9999999"))  <= chk_docnoFr    /* Check From */
/*[UNCALLED]*/                   AND  DECI(DocRunn.DocPref + STRING(DocRunn.DocnoTo,"9999999"))  >= chk_docnoFr ))
/*[UNCALLED]*/                   OR   
/*[UNCALLED]*/                            (DocRunn.Doctyp  = chk_doctype 
/*[UNCALLED]*/                   AND (DECI(DocRunn.DocPref + STRING(DocRunn.DocnoFr,"9999999"))  <= chk_docnoTo    /* Check To   */
/*[UNCALLED]*/                   AND  DECI(DocRunn.DocPref + STRING(DocRunn.DocnoTo,"9999999"))  >= chk_docnoTo )) NO-LOCK NO-ERROR.
/*[UNCALLED]*/         
/*[UNCALLED]*/                 IF AVAIL DocRunn THEN chk_dup = YES. /* Document Dupplicate */
/*[UNCALLED]*/         
/*[UNCALLED]*/             END.
/*[UNCALLED]*/             ELSE DO:
/*[UNCALLED]*/             
/*[UNCALLED]*/                 FIND FIRST  DocRunn USE-INDEX DocRunn03
/*[UNCALLED]*/                   WHERE    (DocRunn.Doctyp  = chk_doctype 
/*[UNCALLED]*/                   AND (DECI(DocRunn.StkPref + STRING(DocRunn.StknoFr,"99999999")) <= chk_docnoFr    /* Check From */
/*[UNCALLED]*/                   AND  DECI(DocRunn.StkPref + STRING(DocRunn.StknoTo,"99999999")) >= chk_docnoFr ))
/*[UNCALLED]*/                   OR   
/*[UNCALLED]*/                            (DocRunn.Doctyp  = chk_doctype 
/*[UNCALLED]*/                   AND (DECI(DocRunn.StkPref + STRING(DocRunn.StknoFr,"99999999")) <= chk_docnoTo    /* Check To   */
/*[UNCALLED]*/                   AND  DECI(DocRunn.StkPref + STRING(DocRunn.StknoTo,"99999999")) >= chk_docnoTo )) NO-LOCK NO-ERROR .
/*[UNCALLED]*/         
/*[UNCALLED]*/                 IF AVAIL DocRunn THEN chk_dup = YES.  /* Sticker Dupplicate */
/*[UNCALLED]*/         
/*[UNCALLED]*/             END.
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         
/*[UNCALLED]*/         /* Call Program Send mail */
/*[UNCALLED]*/         PROCEDURE PD_GenQMail.
/*[UNCALLED]*/         
/*[UNCALLED]*/            DEF INPUT  PARAMETER  pv_DocQuota    AS DECI.
/*[UNCALLED]*/            DEF INPUT  PARAMETER  pv_docyr       AS INT.
/*[UNCALLED]*/            DEF INPUT  PARAMETER  pv_doctype     AS CHAR.
/*[UNCALLED]*/            DEF INPUT  PARAMETER  pv_doccate     AS CHAR.
/*[UNCALLED]*/            DEF INPUT  PARAMETER  pv_doclot      AS INT.
/*[UNCALLED]*/         
/*[UNCALLED]*/            DEF  VAR  nv_mail        AS CHAR INIT "".
/*[UNCALLED]*/            DEF  VAR  nv_mailcc      AS CHAR INIT "".
/*[UNCALLED]*/            DEF  VAR  nv_bcName      AS CHAR INIT "".
/*[UNCALLED]*/            DEF  VAR  nv_patch       AS CHAR INIT "".    
/*[UNCALLED]*/         
/*[UNCALLED]*/            /* Check Document Quota ¤§àËÅ×Í·Ñé§ËÁ´·ÕèÁÕàºÔ¡äÇé ÃÇÁ¡Ñº lot ¶Ñ´ä» ¶éÒÁÕ */
/*[UNCALLED]*/            FIND LAST DocRunn USE-INDEX DocRunn01
/*[UNCALLED]*/                WHERE DocRunn.DocYr   = pv_docyr  
/*[UNCALLED]*/                AND   DocRunn.DocTyp  = pv_doctype
/*[UNCALLED]*/                AND   DocRunn.CateCd  = pv_doccate
/*[UNCALLED]*/                AND   DocRunn.Doclot  = pv_doclot + 1 NO-LOCK NO-ERROR NO-WAIT.
/*[UNCALLED]*/            IF AVAIL DocRunn THEN DO:  /* ¶éÒÁÕáÊ´§ÁÕ¡ÒÃ Set áÅéÇáµè Running àµçÁ */
/*[UNCALLED]*/                IF pv_doctype = "T" THEN pv_DocQuota = pv_DocQuota + (DECI(DocRunn.Stkpref + DocRunn.stknoto) - DECI(DocRunn.stkNext)).
/*[UNCALLED]*/                                    ELSE pv_DocQuota = pv_DocQuota + (DECI(DocRunn.Docpref + DocRunn.docnoto) - DECI(DocRunn.docNext)).
/*[UNCALLED]*/            END.
/*[UNCALLED]*/         
/*[UNCALLED]*/            /* - §Ò¹ Web ·ÕèµéÍ§ÁÕ¡ÒÃàºÔ¡àÍ¡ÊÒÃãËéµÑÇá·¹  à¾ÔèÁãËéá¨é§àµ×Í¹·Ò§ mail ¡Ã³Õ ¨Ó¹Ç¹àÍ¡ÊÒÃàËÅ×Í¹éÍÂ 
/*[UNCALLED]*/               µÒÁ¨Ó¹Ç¹·Õè¡ÓË¹´  
/*[UNCALLED]*/               
/*[UNCALLED]*/               ÃÐºº Premium äÁèÁÕ¡ÒÃ Set Branch , Company Code ·Õè Table Company ¨ÐäÁèà¢éÒ Loop ¹Õé
/*[UNCALLED]*/            -----------------------------------------------------------------------------------------*/
/*[UNCALLED]*/            /* ¶éÒ¨Ó¹Ç¹àÍ¡ÊÒÃ¤§àËÅ×ÍµÒÁ·Õè¡ÓË¹´ãËé Gen Queue Send mail á¨é§àµ×Í¹ BU. µÒÁ Mail ·Õè Set äÇé*/
/*[UNCALLED]*/            IF pv_DocQuota = 500 OR pv_DocQuota = 250 OR pv_DocQuota = 70 OR pv_DocQuota = 50 THEN DO:
/*[UNCALLED]*/            
/*[UNCALLED]*/                FIND FIRST Company USE-INDEX company01
/*[UNCALLED]*/                    WHERE company.compno = gv_bc NO-LOCK NO-ERROR.
/*[UNCALLED]*/                IF AVAIL Company THEN DO:
/*[UNCALLED]*/                    ASSIGN
/*[UNCALLED]*/                        nv_mail     = Company.EMail
/*[UNCALLED]*/                        nv_mailcc   = Company.Internet
/*[UNCALLED]*/                        nv_bcName   = Company.Name.
/*[UNCALLED]*/         
/*[UNCALLED]*/                    OUTPUT TO D:\TEMP\SendMailDocQuota-Log.TXT APPEND.
/*[UNCALLED]*/                    PUT TODAY FORMAT "99/99/9999" " " STRING(TIME,"HH:MM:SS") "." 
/*[UNCALLED]*/                            "Company : " gv_bc     FORMAT "X(12)"  " : " nv_bcName FORMAT "X(45)"
/*[UNCALLED]*/                           " Send To : " nv_mail   FORMAT "X(50)"
/*[UNCALLED]*/                             " CC To : " nv_mailCC FORMAT "X(50)"
/*[UNCALLED]*/                             "Quota Balance : " STRING(pv_docQuota)    FORMAT "X(5)"  SKIP.
/*[UNCALLED]*/                    OUTPUT CLOSE.
/*[UNCALLED]*/             
/*[UNCALLED]*/                    RUN WSU/WSUGSEND.P (INPUT  pv_DocQuota,
/*[UNCALLED]*/                                               gv_DocType ,
/*[UNCALLED]*/                                               nv_mail    ,
/*[UNCALLED]*/                                               nv_mailcc  ,
/*[UNCALLED]*/                                               gv_bc      ,
/*[UNCALLED]*/                                               nv_bcName  ,
/*[UNCALLED]*/                                               gv_usrid   ,
/*[UNCALLED]*/                                        OUTPUT nv_patch   ).
/*[UNCALLED]*/                END.
/*[UNCALLED]*/            END.
/*[UNCALLED]*/         END.
/*[UNCALLED]*/         
/*[UNCALLED]*/         
/*[UNCALLED]*/         
