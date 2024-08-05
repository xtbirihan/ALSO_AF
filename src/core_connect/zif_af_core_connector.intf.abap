interface ZIF_AF_CORE_CONNECTOR
  public .


  methods DELIVER
    importing
      !IS_FORMOUTPUT type FPFORMOUTPUT
    raising
      ZCX_AF_CORE_OUTPUT .
endinterface.
