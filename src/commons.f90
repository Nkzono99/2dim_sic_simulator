module commons
    use vectors

    type(Vector3d), allocatable, target :: e(:, :), b(:, :)
end module