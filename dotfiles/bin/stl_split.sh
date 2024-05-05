#!/usr/bin/env python

import sys

if 'stl' not in dir():
    import stl
for m in stl.mesh.Mesh.from_multi_file(sys.argv[1]):
    m.name=m.name.decode();
    m.save(m.name.strip('\"\"')+'.stl', mode=stl.Mode.ASCII)
