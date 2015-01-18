# A model formulated in JSON code
# NOTE: Since JSON code is not allowd in the "data" folder of an R package
#       the entire JSON code is stored in a string variable.
toymodel='
{
  "comments" : "Note the use of a dummy variable in the definition of the flow process rate",
  "auxx" : {
     "lim_do" : "c_do/(c_do + h_do)"
  },
  "proc" : {
    "flow": "q_in - q_ex + 0.0*v",
    "imex": "q_in / v",
    "degr": "k_deg * c_z * lim_do",
    "surf": "k2(wind,depth) * (DOSAT(temp) - c_do)"
  },
  "stox" : {
    "v" :    {
      "flow" : "1"
    },
    "c_z" :    {
      "imex" : "(c_z_in - c_z)",
      "degr" : "-1"
    },
    "c_do" :    {
      "imex" : "(c_do_in - c_do)",
      "degr" : "-s_do_z",
      "surf" : "1"
    }
  }
}
'
