import os
import time
import random
import plyvel
import json
from math import pi, cos, sin
from collections import namedtuple


Settings = namedtuple("Settings", [
    "asteroid_count",
    "asteroid_size",
    "asteroid_durability",
    "asteroid_rotation_speed",
    "asteroid_field_radius",
    "asteroid_field_position",
    "asteroid_trajectory_probability",

    "gate_position",
    "gate_rotation_speed",
    "gate_size",

    "cp_position",
    "cp_size",
    "cp_durability"
])



def get_counter():
    c = [0]
    def counter():
        c[0] += 1
        return c[0]
    return counter


def ms():
    return int(time.time() * 1000)


def create_gate(db, counter, s):
    ident = "gate:" + str(counter())
    pos = s.gate_position
    angle = random.randint(0, 360)
    rotation = {'rotSpeed': s.gate_rotation_speed,
                'startTs': ms(),
                'startAngle': angle,
                'tag': "EternalRotation"}
    data = {"id": ident,
            "name": "Gate",
            "pos": pos,
            "angle": angle,
            "actions": [rotation],
            "size": [s.gate_size, s.gate_size]}
    db.put(ident, json.dumps(data))


def create_asteroids(db, counter, s):
    assets = ["asteroid_1",
              "asteroid_2",
              "asteroid_3",
              "asteroid_4"]
    c_pos = s.asteroid_field_position
    for _ in xrange(s.asteroid_count):
        ident = "asteroid:" + str(counter())
        len_ = random.uniform(*s.asteroid_field_radius)
        angle = random.uniform(-pi, pi)
        pos = (int(c_pos[0] + len_ * cos(angle)),
               int(c_pos[1] + len_ * sin(angle)))
        size = random.randint(*s.asteroid_size)
        durability = random.randint(*s.asteroid_durability)

        rotation = {'rotSpeed': random.randint(*s.asteroid_rotation_speed),
                    'startTs': ms(),
                    'startAngle': random.randint(0, 360),
                    'tag': "EternalRotation"}
        trajectory = {'center': pos,
                      'radius': max(s.asteroid_field_radius),
                      'rotSpeed': random.randint(*s.asteroid_rotation_speed),
                      'startTs': ms(),
                      'startAngle': random.randint(0, 360),
                      'tag': "MoveCircularTrajectory"}
        actions = []
        actions.append(rotation)
        if random.random() < s.asteroid_trajectory_probability:
            actions.append(trajectory)
        data = {"id": ident,
                "name": "Asteroid",
                "pos": pos,
                "angle": random.randint(0, 360),
                "max-durability": durability,
                "durability": durability,
                "actions": actions,
                "size": [size, size],
                "asset": random.choice(assets)}
        db.put(ident, json.dumps(data))


def create_control_point(db, counter, s):
    ident = "cp:" + str(counter())
    pos = s.cp_position
    actions = []
    data = {"id": ident,
            "name": "Control point",
            "pos": pos,
            "angle": random.randint(0, 360),
            "max-durability": s.cp_durability,
            "durability": s.cp_durability,
            "actions": actions,
            "size": [s.cp_size, s.cp_size],
            "owner": None}
    db.put(ident, json.dumps(data))


def generate(db_dir, area_db_names, settings):
    for area_db_name in area_db_names:
        path = os.path.join(db_dir, area_db_name)
        db = plyvel.DB(path, create_if_missing=True)
        counter = get_counter()
        with db.write_batch() as wb:
            create_gate(wb, counter, settings)
            create_asteroids(wb, counter, settings)
            create_control_point(wb, counter, settings)
        db.close()

