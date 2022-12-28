INTERFACE zif_prvd_file
  PUBLIC .

  TYPES: BEGIN OF ty_filecontent,
           rec TYPE xstring,
           END OF ty_filecontent.

  TYPES: tty_filecontent TYPE TABLE OF ty_filecontent.

ENDINTERFACE.
