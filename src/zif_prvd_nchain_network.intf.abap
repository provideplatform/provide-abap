INTERFACE zif_prvd_nchain_network
  PUBLIC .

  TYPES: BEGIN OF ty_prvd_nchain_network,
           network_id   TYPE zprvd_nchain_networkid,
           network_type TYPE zprvd_nchain_network_type,
           description  TYPE char100,
         END OF ty_prvd_nchain_network.

  TYPES: ty_prvd_nchain_network_list TYPE TABLE OF ty_prvd_nchain_network WITH KEY network_id.

  CLASS-METHODS create IMPORTING is_prvd_nchain_network TYPE ty_prvd_nchain_network.
  CLASS-METHODS read IMPORTING iv_network_id                 TYPE zprvd_nchain_networkid
                     RETURNING VALUE(rs_prvd_nchain_network) TYPE ty_prvd_nchain_network.
  CLASS-METHODS update IMPORTING is_prvd_nchain_network TYPE ty_prvd_nchain_network.
  CLASS-METHODS delete IMPORTING iv_network_id                 TYPE zprvd_nchain_networkid
                       RETURNING VALUE(rs_prvd_nchain_network) TYPE ty_prvd_nchain_network.
  CLASS-METHODS query IMPORTING iv_network_id                 TYPE zprvd_nchain_networkid
                                iv_network_type               TYPE zprvd_nchain_network_type
                                iv_description                TYPE char100
                      RETURNING VALUE(rt_prvd_nchain_network) TYPE ty_prvd_nchain_network_list.

ENDINTERFACE.
