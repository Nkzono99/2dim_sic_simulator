module m_vectors
    use m_vector2d
    use m_vector3d

    private
    public Vector2d
    public Vector3d
    public dot2d
    public dot3d
    public cross2d
    public cross3d

    public operator(+), operator(-), operator(*), operator(/)
    public assignment(=)
end module
