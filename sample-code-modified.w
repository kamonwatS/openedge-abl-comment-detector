/*[COMMENT]*/                    /* Header block comment: should be ignored
/*[COMMENT]*/                       RUN ignored1.p. 
/*[COMMENT]*/                       Total lines: 132, Loc: 41, Comments: 64, Empty: 27
/*[COMMENT]*/                    */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN "app/main.p".       /* this one must be picked up (keep-1) */
/*[BLANK]*/                      
/*[COMMENT]*/                    // RUN ignored2.p       -- line comment, ignored
/*[BLANK]*/                      
/*[COMMENT]*/                    # RUN ignored3.i        -- hash comment, ignored
/*[BLANK]*/                      
/*[COMMENT]*/                    /* block start
/*[COMMENT]*/                    RUN shouldNot.p
/*[COMMENT]*/                    still comment */
/*[BLANK]*/                             
/*[EXECUTABLE]*/                 /* inline block: RUN skipInline.w */  RUN keep-2.i
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN  keep-3.W            /* capital "W" extension  */
/*[BLANK]*/                      
/*[COMMENT]*/                        /* multi-line comment start
/*[COMMENT]*/                    RUN alsoIgnored.p
/*[COMMENT]*/                    continues here
/*[COMMENT]*/                        */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN "keep-1.p"           /* duplicate (different case) */
/*[BLANK]*/                      
/*[EXECUTABLE]*/                 RUN path/to/keep-4.p     /* ordinary .p file */
/*[BLANK]*/                      
/*[COMMENT]*/                    /* RUN PD_GenDataIntReSend. */  
/*[BLANK]*/                      
/*[UNCALLED]*/                   PROCEDURE PD_GenDataIntReSend :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     
/*[UNCALLED]*/                     Parameters:  <none>
/*[UNCALLED]*/                     Notes:    
/*[UNCALLED]*/                     Empty Lines: 6
/*[UNCALLED]*/                     Loc: 24
/*[UNCALLED]*/                     Comment: 8
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   CREATE IntTA68.
/*[UNCALLED]*/                   ASSIGN
/*[UNCALLED]*/                       IntTA68.SystemRq            = IntPol68.SystemRq    
/*[UNCALLED]*/                       IntTA68.MethodCode          = IntPol68.MethodCode  
/*[UNCALLED]*/                       IntTA68.Policy              = IntPol68.Policy      
/*[UNCALLED]*/                       IntTA68.Rencnt              = IntPol68.Rencnt      
/*[UNCALLED]*/                       IntTA68.Endcnt              = IntPol68.Endcnt      
/*[UNCALLED]*/                       IntTA68.PolicyTypeCd        = IntPol68.PolicyTypeCd
/*[UNCALLED]*/                       IntTA68.RateGroup           = IntPol68.RateGroup   
/*[UNCALLED]*/                       IntTA68.PlanCode            = IntPol68.PlanCode   
/*[UNCALLED]*/                   
/*[UNCALLED]*/                       IntTA68.CompanyCode          = IntPol68.CompanyCode
/*[UNCALLED]*/                       IntTA68.BranchCd             = IntPol68.BranchCd   
/*[UNCALLED]*/                       IntTA68.PolicyStatus         = IntPol68.PolicyStatus
/*[UNCALLED]*/                       
/*[UNCALLED]*/                       IntTA68.PolicyNumber         = IntPol68.PolicyNumber
/*[UNCALLED]*/                       IntTA68.PreviousPolicyNumber = IntPol68.PreviousPolicyNumber
/*[UNCALLED]*/                       IntTA68.DocumentUID          = nv_Mdocno1  /*IntPol68.DocumentUID   */
/*[UNCALLED]*/                       
/*[UNCALLED]*/                       IntTA68.ContractNumber       = IntPol68.ContractNumber
/*[UNCALLED]*/                       IntTA68.ContractDt           = IntPol68.ContractDt    
/*[UNCALLED]*/                       IntTA68.ContractTime         = IntPol68.ContractTime  
/*[UNCALLED]*/                       
/*[UNCALLED]*/                       IntTA68.Statusflag          = IntPol68.Statusflag     
/*[UNCALLED]*/                       IntTA68.ResponseResult      = IntPol68.ResponseResult 
/*[UNCALLED]*/                       IntTA68.ResultStatus        = IntPol68.ResultStatus
/*[UNCALLED]*/                       
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[BLANK]*/                      
/*[UNCALLED]*/                   PROCEDURE PD_GenDataIntReSend2 :
/*[UNCALLED]*/                   /*------------------------------------------------------------------------------
/*[UNCALLED]*/                     Purpose:     Never running this procedure
/*[UNCALLED]*/                     Parameters:  <none>   
/*[UNCALLED]*/                     Empty Lines: 1
/*[UNCALLED]*/                     Loc: 12
/*[UNCALLED]*/                     Comment: 7
/*[UNCALLED]*/                   ------------------------------------------------------------------------------*/
/*[UNCALLED]*/                   CREATE IntTA68.
/*[UNCALLED]*/                   ASSIGN
/*[UNCALLED]*/                       IntTA68.SystemRq            = IntPol68.SystemRq    
/*[UNCALLED]*/                       IntTA68.MethodCode          = IntPol68.MethodCode  
/*[UNCALLED]*/                       IntTA68.Policy              = IntPol68.Policy      
/*[UNCALLED]*/                       IntTA68.Rencnt              = IntPol68.Rencnt      
/*[UNCALLED]*/                       IntTA68.Endcnt              = IntPol68.Endcnt      
/*[UNCALLED]*/                       IntTA68.PolicyTypeCd        = IntPol68.PolicyTypeCd
/*[UNCALLED]*/                       IntTA68.RateGroup           = IntPol68.RateGroup   
/*[UNCALLED]*/                       IntTA68.PlanCode            = IntPol68.PlanCode   
/*[UNCALLED]*/                   
/*[UNCALLED]*/                   END PROCEDURE.
/*[BLANK]*/                      
/*[COMMENT]*/                    /*
/*[COMMENT]*/                    ---
/*[COMMENT]*/                    This is a comment, It should be ignored
/*[COMMENT]*/                    ---
/*[COMMENT]*/                    CREATE TB-RESPonseJourney.
/*[BLANK]*/                      
/*[COMMENT]*/                    ASSIGN
/*[COMMENT]*/                    TB-RESPonseJourney.InsurerId               = "STY"
/*[COMMENT]*/                    TB-RESPonseJourney.CompanyCode             = ""
/*[COMMENT]*/                    TB-RESPonseJourney.ContractNumber          = ""
/*[COMMENT]*/                    TB-RESPonseJourney.PolicyNumber            = ""
/*[COMMENT]*/                    TB-RESPonseJourney.Sequence                = ""
/*[COMMENT]*/                    TB-RESPonseJourney.InsuredRefTitle         = ""
/*[COMMENT]*/                    TB-RESPonseJourney.InsuredRefName          = ""
/*[BLANK]*/                      
/*[COMMENT]*/                    TB-RESPonseJourney.InsuredRefPlanCode      = ""
/*[COMMENT]*/                    TB-RESPonseJourney.InsuredRefBirthDt       = ""
/*[BLANK]*/                      
/*[COMMENT]*/                    TB-RESPonseJourney.JourneyProvinceStart    = ""
/*[COMMENT]*/                    TB-RESPonseJourney.JourneyProvinceEnd      = ""
/*[COMMENT]*/                    TB-RESPonseJourney.JourneyStartDt          = ""
/*[COMMENT]*/                    TB-RESPonseJourney.JourneyEndDt            = ""
/*[BLANK]*/                      
/*[COMMENT]*/                    TB-RESPonseJourney.RecordGUIDRs            = ""
/*[COMMENT]*/                    TB-RESPonseJourney.TransactionResponseDt   = STRING( YEAR(TODAY),"9999")
/*[COMMENT]*/                                                                + STRING(MONTH(TODAY),"99") 
/*[COMMENT]*/                                                                + STRING(  DAY(TODAY),"99")
/*[COMMENT]*/                    TB-RESPonseJourney.TransactionResponseTime = STRING(TIME,"HH:MM:SS")
/*[COMMENT]*/                    TB-RESPonseJourney.MsgStatusCd             = "FAIL"
/*[COMMENT]*/                    TB-RESPonseJourney.ErrorMessage            = nv_msgstatus.
/*[BLANK]*/                      
/*[COMMENT]*/                    Nested comment should be ignored.
/*[COMMENT]*/                    /*----- Tantawan ----
/*[COMMENT]*/                    Output Result   Ѻ ¡ ҡ    Policy ---*/
/*[COMMENT]*/                    RUN WRS/SendReqP4Ins.P (INPUT  nv_URL2
/*[COMMENT]*/                                            ,INPUT  nv_node-nameHeader2
/*[COMMENT]*/                                            ,OUTPUT ResponseResult2).
/*[COMMENT]*/                    /**/
/*[BLANK]*/                      
/*[COMMENT]*/                    SendReqInsDetailResult = ResponseResult2.
/*[COMMENT]*/                    */
