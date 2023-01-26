interface ZIF_PRVD_FILE
  public .


  types:
    BEGIN OF ty_filecontent,
           rec TYPE xstring,
           END OF ty_filecontent .
  types:
    tty_filecontent TYPE TABLE OF ty_filecontent .
endinterface.
