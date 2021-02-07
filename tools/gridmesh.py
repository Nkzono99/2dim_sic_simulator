import math

import numpy as np

from mesh import Face, Mesh, Vertex


def create_gridmesh(nx, ny, rate=1.0):
    vertices = []
    for i in range(0, nx+1):
        for j in range(0, ny+1):
            position = [i, j]
            # velocity = 2 * (np.random.rand(2) - 0.5)
            velocity = np.array([0.0, 0.0])
            vert = Vertex(position, velocity, id=len(vertices))
            vertices.append(vert)

    faces = []
    for i in range(nx):
        for j in range(ny):
            i1 = i * (ny+1) + j
            i2 = (i+1) * (ny+1) + j
            i3 = i * (ny+1) + (j+1)
            face = Face(periodic=False,
                        iverts=[i1, i2, i3],
                        rate=2*rate*np.random.rand())
            faces.append(face)

    for i in range(1, nx+1):
        for j in range(1, ny+1):
            i1 = i * (ny+1) + j
            i2 = (i-1) * (ny+1) + j
            i3 = i * (ny+1) + (j-1)
            face = Face(periodic=False,
                        iverts=[i1, i2, i3],
                        rate=2*rate*np.random.rand())
            faces.append(face)

    return Mesh(vertices, faces)


def create_gridmesh_periodic(nx, ny, rate=1.0):
    mesh = create_gridmesh(nx+2, ny+2, rate=rate)
    for vert in mesh.vertices:
        vert.position[0] = vert.position[0] - 1
        vert.position[1] = vert.position[1] - 1

    posdict = {}
    for i, vert1 in enumerate(mesh.vertices):
        x1 = vert1.position[0] % nx
        y1 = vert1.position[1] % ny
        cood = (x1, y1)

        if cood in posdict:
            vert1.id = posdict[cood].id
        else:
            posdict[cood] = vert1

    return mesh.correct_periodic(nx, ny)
