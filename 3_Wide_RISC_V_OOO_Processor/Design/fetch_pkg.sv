`ifndef FETCH_TYPES_PKG_SV
`define FETCH_TYPES_PKG_SV

package fetch_types_pkg;

    // Fetch entry: used for instruction queue, fetch-to-decode latch, etc.
    typedef struct packed {
        logic [31:0] instruction;
        logic [31:0] pc;
        logic        valid;
    } fetch_entry_t;

endpackage : fetch_types_pkg

`endif // FETCH_TYPES_PKG_SV 