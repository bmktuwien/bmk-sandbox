from sqlalchemy import *
from sqlalchemy.orm import *
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


class A(Base):
    __tablename__ = "a"

    id = Column(Integer, primary_key=True)
    bs = relationship("B")

    def __str__(self):
        return "A: %s" % dict({'id': self.id})

    def __repr__(self):
        return "A: %s" % dict({'id': self.id})


class B(Base):
    __tablename__ = "b"

    id = Column(Integer, primary_key=True)

    a_id = Column(Integer, ForeignKey('a.id'))

    def __str__(self):
        return "B: %s" % dict({'id': self.id, 'a_id': self.a_id})

    def __repr__(self):
        return "B: %s" % dict({'id': self.id, 'a_id': self.a_id})

##############################################################################
##############################################################################
##############################################################################

# global vars
engine = None


def init():
    global engine

    engine = create_engine("sqlite://", echo=True)
    Base.metadata.create_all(engine)
    s = Session(engine)
    s.add_all([A(bs=[B(), B()]), A(bs=[B()])])
    s.commit()


def main():
    init()
    session = Session(engine)

    # perform some basic queries
    all_as = session.query(A).all()
    all_bs = session.query(B).all()

    for a in all_as:
        print("%s" % a)

    for b in all_bs:
        print("%s" % b)

    # select multiple entities (variant 1)
    query = session.query(A, B).filter(A.id == B.a_id)

    for e in query.all():
        print(str(e))

    # select multiple entities (variant 2)
    query = session.query(A)\
        .join(B, A.id == B.a_id)\
        .with_entities(A, B)

    for e in query.all():
        print(str(e))


if __name__ == "__main__":
    main()