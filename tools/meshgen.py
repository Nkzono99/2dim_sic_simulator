import gridmesh
from argparse import ArgumentParser
import numpy as np


def parse_args():
    parser = ArgumentParser()

    parser.add_argument('--meshtype', '-mt', default='grid')
    parser.add_argument('--npc', '--npc', type=int, default=1)
    parser.add_argument('--range', type=int, nargs=2, default=(10, 10))
    parser.add_argument('--rate', '-r', type=float, default=1.0)
    parser.add_argument('--output', '-o', default='mesh.msh')
    parser.add_argument('--noperiodic', action='store_true')
    parser.add_argument('--plot', action='store_true')

    return parser.parse_args()


def create_mesh(nx, ny, rate, velocity, periodic):
    if not periodic:
        mesh = gridmesh.create_gridmesh(nx, ny, rate)
    else:
        mesh = gridmesh.create_gridmesh_periodic(nx, ny, rate)
    dx = 1 / nx
    dy = 1 / ny

    if velocity is not None:
        for vert in mesh.vertices:
            vert.velocity = velocity
    for vert in mesh.vertices:
        if nx == 1:
            vert.velocity[0] = 0
        if ny == 1:
            vert.velocity[1] = 0
    for vert in mesh.vertices:
        vert.position = (np.random.rand(2) - 0.5) + np.array(vert.position)

    mesh.finalize(dx, dy)
    return mesh


args = parse_args()

if args.meshtype == 'grid':
    nx, ny = args.range

    mesh = create_mesh(nx, ny, 0.5, velocity=None,
                       periodic=not args.noperiodic)

    mesh.save(args.output)
    if args.plot:
        mesh.plot(show=True)

elif args.meshtype == 'grid2':
    nx, ny = args.range

    mesh1 = create_mesh(nx, ny, 0.25, velocity=[1, 0],
                        periodic=not args.noperiodic)
    mesh2 = create_mesh(nx, ny, 0.25, velocity=[-1, 0],
                        periodic=not args.noperiodic)

    mesh = mesh1 + mesh2
    mesh.save(args.output)
    if args.plot:
        mesh.plot(show=True)
