# -*- coding: utf-8 -*-

from setuptools import setup

setup(
    name='sqlalchemy-tutorial',
    version='0.0.1',
    description="SQLAlchemy Tutorial",
    long_description="""SQLAlchemy Tutorial""",
    author='BMK',
    packages=['tutorial'],
    zip_safe=False,
    install_requires=[
        'SQLAlchemy == 0.8.7'
    ]
)

