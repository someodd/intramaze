# IntraMaze

Random chat encounters in a collaboratively-built maze.

A maze made out of rooms users have made and arbitrarily linked together. Explore the maze, if you bump into the other users in the same room you can have an ephemeral conversation.

I use Debian (unstable).

## How does it work

The website is a static website managed by daemon.

The daemon is a REST API for managing the static website as well as managing the websocket connections for the chatting. REST API authentication is managed with JWT.

## Running

Run the backend:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=MazeQuest cabal run
```

The above will build the static files and run the REST API, which both manages the database and handles updating the static files.

Serve the built static files directory (here's a way to test):

```
cd built
python3 -m http.server
```

Now you can visit http://localhost:8000/.

## Setup/Dependencies

```
sudo apt install libjwt zlib1g-dev postgresql postgresql-contrib libpq-dev libjwt-dev
```

## Set up JWT signature keys

You will also need to generate the private key for JSON Web Tokens:

```
openssl ecparam -name secp256k1 -genkey -noout -out jwt-priv-sig-key.pem
openssl ec -in jwt-priv-sig-key.pem -pubout > jwt-pub-sig-key.pem

```

### Setting up PostgreSQL

```
sudo -u postgres psql
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
```


## `static/`

The `static/mustache-build` are literal pages to build (like copying + running through parser), whereas `static/mustache` is simply for templates that get used to build pages.