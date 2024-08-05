class ZCL_AF_CORE_CONSTANTS definition
  public
  final
  create public .

public section.

  constants GC_ASC_DEVICE_TYP_SHORT type RSPOPNAME value 'ASC' ##NO_TEXT.
  constants GC_DOCID_AVIS type ZAF_DOCID value '0000004' ##NO_TEXT.
  constants GC_DOCID_CONFIG_CENTER type ZAF_DOCID value '0000003' ##NO_TEXT.
  constants GC_DOCID_DELNOTE type ZAF_DOCID value '0000001' ##NO_TEXT.
  constants GC_DOCID_DN_BECHTLE type ZAF_DOCID value '0000005' ##NO_TEXT.
  constants GC_DOCID_DN_BECHTLE_L type ZAF_DOCID value '0000006' ##NO_TEXT.
  constants GC_DOCID_GOLDENFORM type ZAF_DOCID value '0000000' ##NO_TEXT.
  constants GC_DOCID_PARTS_LIST type ZAF_DOCID value '0000007' ##NO_TEXT.
  constants GC_DOCID_PO_LIST type ZAF_DOCID value '0000008' ##NO_TEXT.
  constants GC_DOCID_RETURNS_DEL type ZAF_DOCID value '0000009' ##NO_TEXT.
  constants GC_DOCID_SN_ARC_DOC type ZAF_DOCID value '0000002' ##NO_TEXT.
  constants GC_EMAIL_RECEIVER_SEPARATOR type CHAR1 value ';' ##NO_TEXT.
  constants GC_GROUP_OF_COMPANIES_LANGUAGE type LANGU value 'E' ##NO_TEXT.
  constants GC_LOGGER_OBJECT_AF type BALOBJ_D value 'ZAF' ##NO_TEXT.
  constants GC_LOGGER_SUBOBJECT_ERROR type BALSUBOBJ value 'ERROR' ##NO_TEXT.
  constants GC_LOGGER_TYP_A type BALTMSGTY value 'A' ##NO_TEXT.
  constants GC_LOGGER_TYP_E type BALTMSGTY value 'E' ##NO_TEXT.
  constants GC_LOGGER_TYP_I type BALTMSGTY value 'I' ##NO_TEXT.
  constants GC_LOGGER_TYP_S type BALTMSGTY value 'S' ##NO_TEXT.
  constants GC_LOGGER_TYP_W type BALTMSGTY value 'W' ##NO_TEXT.
  constants GC_LOG_SUBOBJ_CORE_CON type BALSUBOBJ value 'CORE_CONNECT' ##NO_TEXT.
  constants GC_LOG_SUBOBJ_CORE_OUTPUT type BALSUBOBJ value 'CORE_OUTPUT' ##NO_TEXT.
  constants GC_LOG_SUBOBJ_CTRL type BALSUBOBJ value 'CORE_CONTROLLER' ##NO_TEXT.
  constants GC_LOG_SUBOBJ_ENTRY type BALSUBOBJ value 'ENTRY' ##NO_TEXT.
  constants GC_LOG_SUBOBJ_SPEC type BALSUBOBJ value 'SPEC' ##NO_TEXT.
  constants GC_MIME_REPOSITORY_LOGO_PATH type SKWF_URL value '/SAP/BC/fp/graphics/PUBLIC/ADOBE_FORMS/' ##NO_TEXT.
  constants GC_MSGID_DEFAULT type SYST_MSGID value 'ZAF_CORE' ##NO_TEXT.
  constants GC_MSGNO_GENERIC type SYST_MSGNO value '999' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_ARCHIVE type ZAF_OUTPUT_CHANNEL value 'ARC' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_FAX type ZAF_OUTPUT_CHANNEL value 'FAX' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_FILE_CO type ZAF_OUTPUT_CHANNEL value 'FIL' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_FTP_CO type ZAF_OUTPUT_CHANNEL value 'FTP' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_MAIL_CO type ZAF_OUTPUT_CHANNEL value 'MAI' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_OUT_INFO type ZAF_OUTPUT_CHANNEL value 'INF' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_PREVIEW type ZAF_OUTPUT_CHANNEL value 'PRV' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_PRINT type ZAF_OUTPUT_CHANNEL value 'PRT' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_PRINT_PCL type ZAF_OUTPUT_CHANNEL value 'PCL' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_PRINT_PS type ZAF_OUTPUT_CHANNEL value 'PPS' ##NO_TEXT.
  constants GC_OUTPUT_CHANNEL_PRINT_ZPL type ZAF_OUTPUT_CHANNEL value 'ZPL' ##NO_TEXT.
  constants GC_PCL_DEVICE_TYP_SHORT type RSPOPNAME value 'PCL' ##NO_TEXT.
  constants GC_PCL_FILE_TYP type RSPOPNAME value 'pcl' ##NO_TEXT.
  constants GC_PDF_DEVICE_TYP_SHORT type RSPOPNAME value 'PDF' ##NO_TEXT.
  constants GC_PDF_FILE_TYP type RSPOPNAME value 'pdf' ##NO_TEXT.
  constants GC_PS_DEVICE_TYP_SHORT type RSPOPNAME value 'PS' ##NO_TEXT.
  constants GC_PS_FILE_TYP type RSPOPNAME value 'ps' ##NO_TEXT.
  constants GC_SAP_DEVICE_MAIL type FPMEDIUM value 'MAIL' ##NO_TEXT.
  constants GC_SAP_DEVICE_PRINTER type FPMEDIUM value 'PRINTER' ##NO_TEXT.
  constants GC_SAP_DEVICE_TELEFAX type FPMEDIUM value 'TELEFAX' ##NO_TEXT.
  constants GC_SET_HEAD type CHAR1 value 'H' ##NO_TEXT.
  constants GC_SET_ITEM type CHAR1 value 'U' ##NO_TEXT.
  constants GC_SYID_T_SYSTEM type SY-SYSID value 'TE1' ##NO_TEXT.
  constants GC_SYID_Q_SYSTEM type SY-SYSID value 'QE1' ##NO_TEXT.
  constants GC_SYID_P_SYSTEM type SY-SYSID value 'PE1' ##NO_TEXT.
  constants GC_NOREPLY_SENDER type AD_SMTPADR value 'noreply@also.com' ##NO_TEXT.
  constants GC_CR_LF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
protected section.
private section.
ENDCLASS.



CLASS ZCL_AF_CORE_CONSTANTS IMPLEMENTATION.
ENDCLASS.
