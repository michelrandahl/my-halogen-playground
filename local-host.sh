#!/usr/bin/env bash

# hosting with python because parcel crashes so often...
echo "hosting at http://localhost:8000/dev/"
python3 -m http.server 8000
