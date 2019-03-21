maybe   = t -> <t>
just    = t -> <t> : maybe <- t
nothing = t -> <>  : maybe <- t
