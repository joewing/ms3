open Base_memory

class virtual main_memory =
    object (self)
        inherit base_memory as super

        val mutable writes = 0

        val mutable reads = 0

        method writes = writes

        method virtual energy : float -> float

        method reset m main =
            super#reset m main;
            writes <- 0;
            reads <- 0

    end
