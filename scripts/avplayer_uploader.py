#!/usr/bin/env python

from __future__ import unicode_literals

import sys
import os
import requests


# globals
video_types = ['mp4', 'avi', 'xvid', 'mkv']


# print the usage message and die
def print_usage():
    print('avplayer_uploader <url> <directory>')
    sys.exit(1)


# return video file names in the given directory
def get_video_files(root_dir):
    global video_types

    for directory, _, files in os.walk(root_dir):
        for f in files:
            vf_types = [t for t in video_types if f.endswith('.'+t)]
            if vf_types:
                yield os.path.join(directory, f), f, vf_types[0]


def upload_video(url, file_path, file_name, file_type):
    files = [
        ('file', (file_name, open(file_path, 'rb'), 'video/'+file_type)),
        ('file', ('button', 'Submit'))
    ]

    if not url.startswith('http://'):
        url = 'http://%s' % url

    print('Uploading %s...' % file_name)
    response = requests.post(url, files=files)

    if response.status_code == 200:
        print('%s successfully uploaded! :-)' % file_name)


def main(args=sys.argv[1:]):
    if len(args) != 2:
        print_usage()

    url, working_dir = args

    for fp, fn, ft in get_video_files(working_dir):
        upload_video(url, fp, fn, ft)


if __name__ == '__main__':
    main()
