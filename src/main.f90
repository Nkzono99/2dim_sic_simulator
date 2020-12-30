program main
    use sic_types

    type(Simplex) :: s
    type(Tracer) :: t

    t = Tracer()

    print *, 'correct', t%position
end program