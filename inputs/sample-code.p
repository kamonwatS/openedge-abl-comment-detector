/* Header block comment: should be ignored
   RUN ignored1.p. 
   Total lines: 132, Loc: 41, Comments: 64, Empty: 27
*/

RUN "app/main.p".       /* this one must be picked up (keep-1) */

// RUN ignored2.p       -- line comment, ignored

# RUN ignored3.i        -- hash comment, ignored

/* block start
RUN shouldNot.p
still comment */
       
/* inline block: RUN skipInline.w */  RUN keep-2.i

RUN  keep-3.W            /* capital "W" extension  */

    /* multi-line comment start
RUN alsoIgnored.p
continues here
    */

RUN "keep-1.p"           /* duplicate (different case) */

RUN path/to/keep-4.p     /* ordinary .p file */

/* RUN PD_GenDataIntReSend. */  

PROCEDURE PD_GenDataIntReSend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  Empty Lines: 6
  Loc: 24
  Comment: 8
------------------------------------------------------------------------------*/
CREATE IntTA68.
ASSIGN
    IntTA68.SystemRq            = IntPol68.SystemRq    
    IntTA68.MethodCode          = IntPol68.MethodCode  
    IntTA68.Policy              = IntPol68.Policy      
    IntTA68.Rencnt              = IntPol68.Rencnt      
    IntTA68.Endcnt              = IntPol68.Endcnt      
    IntTA68.PolicyTypeCd        = IntPol68.PolicyTypeCd
    IntTA68.RateGroup           = IntPol68.RateGroup   
    IntTA68.PlanCode            = IntPol68.PlanCode   

    IntTA68.CompanyCode          = IntPol68.CompanyCode
    IntTA68.BranchCd             = IntPol68.BranchCd   
    IntTA68.PolicyStatus         = IntPol68.PolicyStatus
    
    IntTA68.PolicyNumber         = IntPol68.PolicyNumber
    IntTA68.PreviousPolicyNumber = IntPol68.PreviousPolicyNumber
    IntTA68.DocumentUID          = nv_Mdocno1  /*IntPol68.DocumentUID   */
    
    IntTA68.ContractNumber       = IntPol68.ContractNumber
    IntTA68.ContractDt           = IntPol68.ContractDt    
    IntTA68.ContractTime         = IntPol68.ContractTime  
    
    IntTA68.Statusflag          = IntPol68.Statusflag     
    IntTA68.ResponseResult      = IntPol68.ResponseResult 
    IntTA68.ResultStatus        = IntPol68.ResultStatus
    

END PROCEDURE.


PROCEDURE PD_GenDataIntReSend2 :
/*------------------------------------------------------------------------------
  Purpose:     Never running this procedure
  Parameters:  <none>   
  Empty Lines: 1
  Loc: 12
  Comment: 7
------------------------------------------------------------------------------*/
CREATE IntTA68.
ASSIGN
    IntTA68.SystemRq            = IntPol68.SystemRq    
    IntTA68.MethodCode          = IntPol68.MethodCode  
    IntTA68.Policy              = IntPol68.Policy      
    IntTA68.Rencnt              = IntPol68.Rencnt      
    IntTA68.Endcnt              = IntPol68.Endcnt      
    IntTA68.PolicyTypeCd        = IntPol68.PolicyTypeCd
    IntTA68.RateGroup           = IntPol68.RateGroup   
    IntTA68.PlanCode            = IntPol68.PlanCode   

END PROCEDURE.

/*
---
This is a comment, It should be ignored
---
CREATE TB-RESPonseJourney.

ASSIGN
TB-RESPonseJourney.InsurerId               = "STY"
TB-RESPonseJourney.CompanyCode             = ""
TB-RESPonseJourney.ContractNumber          = ""
TB-RESPonseJourney.PolicyNumber            = ""
TB-RESPonseJourney.Sequence                = ""
TB-RESPonseJourney.InsuredRefTitle         = ""
TB-RESPonseJourney.InsuredRefName          = ""

TB-RESPonseJourney.InsuredRefPlanCode      = ""
TB-RESPonseJourney.InsuredRefBirthDt       = ""

TB-RESPonseJourney.JourneyProvinceStart    = ""
TB-RESPonseJourney.JourneyProvinceEnd      = ""
TB-RESPonseJourney.JourneyStartDt          = ""
TB-RESPonseJourney.JourneyEndDt            = ""

TB-RESPonseJourney.RecordGUIDRs            = ""
TB-RESPonseJourney.TransactionResponseDt   = STRING( YEAR(TODAY),"9999")
                                            + STRING(MONTH(TODAY),"99") 
                                            + STRING(  DAY(TODAY),"99")
TB-RESPonseJourney.TransactionResponseTime = STRING(TIME,"HH:MM:SS")
TB-RESPonseJourney.MsgStatusCd             = "FAIL"
TB-RESPonseJourney.ErrorMessage            = nv_msgstatus.

Nested comment should be ignored.
/*----- Tantawan ----
Output Result   Ѻ ¡ ҡ    Policy ---*/
RUN WRS/SendReqP4Ins.P (INPUT  nv_URL2
                        ,INPUT  nv_node-nameHeader2
                        ,OUTPUT ResponseResult2).
/**/

SendReqInsDetailResult = ResponseResult2.
*/