interface ZIF_PROUBC_FILE
  public .

      TYPES: BEGIN OF ty_filecontent,
           rec TYPE xstring,
           END OF ty_filecontent.

      types: tty_filecontent type table of ty_filecontent.

endinterface.
