CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type TEXT CHECK( type IN ('teacher','ta','student') ) NOT NULL,
    name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS classrooms (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS classroom_participants (
    user_id INTEGER NOT NULL,
    classroom_id INTEGER NOT NULL,
    FOREIGN KEY(user_id) REFERENCES users(id),
    FOREIGN KEY(classroom_id) REFERENCES classrooms(id), 
    PRIMARY KEY (user_id, classroom_id)
);

CREATE TABLE IF NOT EXISTS assignments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    start_date TEXT NOT NULL,
    deadline TEXT NOT NULL,
    description TEXT NOT NULL,
    weight REAL NOT NULL,
    classroom_id INTEGER NOT NULL,
    FOREIGN KEY(classroom_id) REFERENCES classrooms(id)
);

CREATE TABLE IF NOT EXISTS submissions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    assignment_id INTEGER NOT NULL,
    FOREIGN KEY(user_id) REFERENCES users(id),
    FOREIGN KEY(assignment_id) REFERENCES assignments(id)
);

CREATE TABLE IF NOT EXISTS attempts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    submission_id INTEGER NOT NULL,
    FOREIGN KEY(submission_id) REFERENCES submissions(id)
);

CREATE TABLE IF NOT EXISTS gradings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    submission_id INTEGER NOT NULL,
    grade INTEGER NOT NULL,
    user_id INTEGER NOT NULL,
    timestamp TEXT NOT NULL,
    feedback TEXT NOT NULL,
    FOREIGN KEY(submission_id) REFERENCES submissions(id),
    FOREIGN KEY(user_id) REFERENCES users(id)
);