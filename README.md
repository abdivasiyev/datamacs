# datamacs

A database client for Emacs. Write queries in Org source blocks, run
them with `C-c C-c`, browse and edit the results.

Supports SQLite (built into Emacs) and PostgreSQL.

## Requirements

- Emacs 29.1 or newer
- PostgreSQL only: the [pg](https://melpa.org/#/pg) package from MELPA

## Install

From a checkout:

```elisp
(add-to-list 'load-path "~/src/datamacs/lisp")
(require 'datamacs)
```

With Nix, in your Emacs package list:

```nix
(epkgs.trivialBuild rec {
  pname = "datamacs";
  version = "0.0.1";
  src = pkgs.fetchFromGitHub {
    owner = "abdivasiyev";
    repo = "datamacs";
    rev = "v${version}";
    hash = lib.fakeHash; # replace with the hash Nix prints
  };
  sourceRoot = "source/lisp";
  packageRequires = [ epkgs.pg ];
})
```

## Quick start

```org
#+begin_src datamacs :driver sqlite :file ~/notes.db
SELECT * FROM people
#+end_src
```

`C-c C-c` runs the block and opens the result in a window at the
bottom. See [examples/sqlite.org](examples/sqlite.org).

## Connections

Declare connections once:

```elisp
(setq datamacs-connections
      '((:name "notes" :driver sqlite :file "~/notes.db")
        (:name "prod" :driver postgres :host "db.example.com" :port 5432
         :user "app" :database "shop" :sslmode require :readonly t)))
```

Use them by name, per block or for a whole file:

```org
#+PROPERTY: header-args:datamacs :conn prod

#+begin_src datamacs :database analytics
SELECT count(*) FROM events
#+end_src
```

| Key         | Driver   | Meaning                                       |
|-------------|----------|-----------------------------------------------|
| `:file`     | sqlite   | Database file                                 |
| `:host`     | postgres | Server, default `localhost`                   |
| `:port`     | postgres | Port, default `5432`                          |
| `:user`     | postgres | User, default your login name                 |
| `:database` | postgres | Database, default the user name               |
| `:sslmode`  | postgres | `disable`, `prefer` (default) or `require`    |
| `:password` | postgres | Password; better left to authinfo             |
| `:readonly` | all      | `t` allows only reading queries               |
| `:session`  | all      | Share one open connection between blocks      |

Header arguments on a block override the connection's values.
`:readonly` can only be turned on. After editing a `#+PROPERTY` line,
press `C-c C-c` on it.

## Running queries

- `C-c C-c` runs the statement under the cursor, or the selected text.
- `:results replace` writes the result into the Org file instead.
- Each query gets its own result window, so results can be compared.

In a result window:

| Key                  | Action                         |
|----------------------|--------------------------------|
| `RET`, click         | Edit the cell                  |
| `C-u RET`            | Set the cell to NULL           |
| `g`                  | Run the query again, read-only |
| `q`                  | Close                          |

Cells can be edited when the query is a plain `SELECT` from one table
that includes its primary key. An edit only applies if the row still
has the value you saw.

## Sessions and transactions

Blocks without `:session` open and close their own connection. Blocks
with the same `:session` share one, so a transaction can span them:

```org
#+begin_src datamacs :conn prod :session tx
BEGIN;
UPDATE accounts SET balance = 0 WHERE id = 1;
SELECT * FROM accounts WHERE id = 1;
ROLLBACK;
#+end_src
```

Run each statement with the cursor on it. `M-x datamacs-disconnect`
closes all sessions and asks before rolling back open transactions.

## Read-only

On a `:readonly t` connection only `SELECT`, `WITH`, `VALUES`,
`TABLE`, `SHOW` and `EXPLAIN` (without `ANALYZE`) run. PostgreSQL
queries also run in a `READ ONLY` transaction. For production, use a
database user with read rights only.

## Passwords

Passwords come from `:password`, or from auth-source
(`~/.authinfo.gpg`). If the server asks for one that is not stored,
Emacs asks you and offers to save it after a successful login.

```
machine db.example.com login app port 5432 password secret
```

## Schema sidebar

`M-x datamacs-schema` opens the sidebar at the connection of the
block at point. Bind it in Org:

```elisp
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c d" #'datamacs-schema))
```

| Key   | Action                                                 |
|-------|--------------------------------------------------------|
| `RET` | Load a connection or database; preview a table         |
| `TAB` | Fold                                                   |
| `g`   | Read again from the server                             |
| `q`   | Close                                                  |

It shows databases, schemas, tables, columns, constraints, foreign
keys, indexes and partitions. Databases load only when selected. The
schema is cached on disk and also completes table and column names in
blocks with `C-M-i`.

## Settings

| Variable                   | Default | Meaning                                   |
|----------------------------|---------|-------------------------------------------|
| `datamacs-connections`     | `nil`   | Declared connections                      |
| `datamacs-connect-timeout` | `10`    | Seconds to wait for a server              |
| `datamacs-log-queries`     | `t`     | Log sent queries in `*Messages*`          |

## License

GPL-3.0-or-later

## Third-party dependencies

Everything else is built into Emacs. PostgreSQL support uses:

| Package                                   | Author        | License          |
|-------------------------------------------|---------------|------------------|
| [pg](https://github.com/emarsden/pg-el)   | Eric Marsden  | GPL-3.0-or-later |
| [peg](https://elpa.gnu.org/packages/peg.html), needed by pg; built into Emacs 30 | Helmut Eller | GPL-3.0-or-later |
