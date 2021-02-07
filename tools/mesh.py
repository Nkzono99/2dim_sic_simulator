import copy
import json
from typing import List, Tuple

import matplotlib.pyplot as plt
import numpy as np


class Vertex:
    def __init__(self,
                 position: Tuple[float, float],
                 velocity: Tuple[float, float],
                 id: int = 0):

        if isinstance(position, np.ndarray):
            position = list(position)
        if isinstance(velocity, np.ndarray):
            velocity = list(velocity)

        self.position = position
        self.velocity = velocity
        self.id = id

    def todict(self, index):
        return {
            'index': index,
            'position': self.position,
            'velocity': self.velocity
        }

    def tomsh(self, index):
        x, y = self.position
        vx, vy = self.velocity
        return '{},{},{},{},{}'.format(index, x, y, vx, vy)


class Face:
    def __init__(self,
                 periodic: bool,
                 iverts: List[int],
                 rate: float):
        self.periodic = periodic
        self.iverts = iverts
        self.rate = rate

    def todict(self, index):
        return {
            'index': index,
            'periodic': self.periodic,
            'iverts': [ivert + 1 for ivert in self.iverts],
            'rate': self.rate
        }

    def tomsh(self, index):
        return '{},{},{},{},{}'.format(
            index,
            1 if self.periodic else 0,
            self.rate,
            len(self.iverts),
            ','.join(list(map(str, [ivert + 1 for ivert in self.iverts])))
        )


class Mesh:
    def __init__(self,
                 vertices: List[Vertex],
                 faces: List[Face]):
        self.vertices: List[Vertex] = vertices
        self.faces: List[Face] = faces

    def plot(self, show=True):
        ims = []
        for face in self.faces:
            for i in range(len(face.iverts) // 3):
                i1, i2, i3 = face.iverts[i*3:(i+1)*3]

                x1, y1 = self.vertices[i1].position
                x2, y2 = self.vertices[i2].position
                x3, y3 = self.vertices[i3].position

                xs = [x1, x2, x3, x1]
                ys = [y1, y2, y3, y1]

                ims += plt.plot(xs, ys, '.-', color='blue')

        if show:
            plt.show()
        else:
            return ims

    def finalize(self, dx, dy):
        for vert in self.vertices:
            vert.position[0] *= dx
            vert.position[1] *= dy

    def correct_periodic(self, xlim, ylim):
        vertices = copy.deepcopy(self.vertices)
        faces = copy.deepcopy(self.faces)

        idlist = {}
        deletes = [False] * len(faces)
        for i, face1 in enumerate(faces):
            v1 = vertices[face1.iverts[0]]
            v2 = vertices[face1.iverts[1]]
            v3 = vertices[face1.iverts[2]]
            idset = frozenset((v1.id, v2.id, v3.id))

            face1.iverts = sorted(face1.iverts, key=lambda iv: vertices[iv].id)
            x1, y1 = vertices[face1.iverts[0]].position
            x2, y2 = vertices[face1.iverts[1]].position
            x3, y3 = vertices[face1.iverts[2]].position
            cross = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)
            if cross < 0:
                tmp = face1.iverts[2]
                face1.iverts[2] = face1.iverts[1]
                face1.iverts[1] = tmp

            all_in = (0 <= x1 and x1 < xlim) and \
                     (0 <= x2 and x2 < xlim) and \
                     (0 <= x3 and x3 < xlim) and \
                     (0 <= y1 and y1 < ylim) and \
                     (0 <= y2 and y2 < ylim) and \
                     (0 <= y3 and y3 < ylim)

            if idset in idlist:
                face2 = idlist[idset]
                if all_in:
                    face2.iverts = face1.iverts + face2.iverts
                else:
                    face2.iverts += face1.iverts
                face2.periodic = True
                deletes[i] = True
            else:
                idlist[idset] = face1

        faces = [face for i, face in enumerate(faces) if not deletes[i]]

        return Mesh(vertices, faces)

    def save(self, filename: str):
        nverts = len(self.vertices)
        nfaces = len(self.faces)
        with open(filename, 'wt', encoding='utf-8') as f:
            f.write('{},{}\n'.format(nverts, nfaces))
            for i, vert in enumerate(self.vertices):
                f.write(vert.tomsh(i+1))
                f.write('\n')
            for i, face in enumerate(self.faces):
                f.write(face.tomsh(i+1))
                f.write('\n')

    def save_json(self, filename: str):
        data = {}
        data['nverts'] = len(self.vertices)
        data['nfaces'] = len(self.faces)

        vertex_list = []
        for i, vert in enumerate(self.vertices):
            vertex_list.append(vert.todict(i+1))
        data['vertex_list'] = vertex_list

        face_list = []
        for i, face in enumerate(self.faces):
            face_list.append(face.todict(i+1))
        data['face_list'] = face_list

        with open(filename, 'wt', encoding='utf-8') as f:
            print('save', filename)
            json.dump(data, f)

    def __add__(self, other):
        vertices1 = copy.deepcopy(self.vertices)
        vertices2 = copy.deepcopy(other.vertices)
        faces1 = copy.deepcopy(self.faces)
        faces2 = copy.deepcopy(other.faces)

        idmax = 0
        for vert1 in vertices1:
            idmax = max(idmax, vert1.id)
        for vert2 in vertices2:
            vert2.id += idmax + 1

        nverts1 = len(vertices1)
        for face2 in faces2:
            face2.iverts = [ivert2 + nverts1 for ivert2 in face2.iverts]

        return Mesh(vertices1+vertices2, faces1+faces2)
