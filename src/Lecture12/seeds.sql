CREATE TABLE IF NOT EXISTS movie_sessions
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , title TEXT NOT NULL
  , start_time DATETIME NOT NULL
  , duration INTEGER NOT NULL
  );

CREATE TABLE IF NOT EXISTS seats
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , row INTEGER NOT NULL
  , seat INTEGER NOT NULL
  , available BOOLEAN NOT NULL
  , movie_session_id INTEGER REFERENCES movie_sessions(id)
  );

CREATE TABLE IF NOT EXISTS bookings
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , seat_id INTEGER REFERENCES seats(id)
  , movie_session_id INTEGER REFERENCES movie_sessions(id)
  , is_preliminary BOOLEAN NOT NULL
  , created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , UNIQUE(seat_id, movie_session_id)
  );

INSERT INTO movie_sessions (title, start_time, duration)
VALUES ('John Wick 3', '2020-05-11 19:11:23', 120);

INSERT INTO seats (row, seat, available, movie_session_id)
VALUES (1, 3, true, 1);
INSERT INTO seats (row, seat, available, movie_session_id)
VALUES (2, 3, true, 1);
INSERT INTO seats (row, seat, available, movie_session_id)
VALUES (3, 3, false, 1);
