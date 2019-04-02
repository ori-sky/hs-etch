maybe   = t      -> <t>
nothing = t      -> <>      : maybe <- t
just    = (t, x) -> <x : t> : maybe <- t
