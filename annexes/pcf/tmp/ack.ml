letrec ack = 
  \m.\n.ifz m then n+1 
        else ifz n then ack (m-1) 1 
        else ack (m-1) (ack m (n-1)) 
in ack 3 2.

