#!/usr/bin/env bash

# SSL stuff... do I need to put this somewhere in a volume to persist the
# certificate? I think so!
# FIXME: these commands should only be ran once! don't overwrite...
echo "what the heck"
if [ "$SCOTTY_ENV" != "Production" ]; then
mkdir -p /etc/letsencrypt/live/interwebz.earth/
    #certbot --nginx -d interwebz.earth --test-cert --no-verify-ssl --non-interactive --agree-tos -m someodd@pm.me
openssl req -x509 -out /etc/letsencrypt/live/interwebz.earth/fullchain.crt -keyout /etc/letsencrypt/live/interwebz.earth/privkey.pem \
  -newkey rsa:2048 -nodes -sha256 \
  -subj '/CN=localhost' -extensions EXT -config <( \
   printf "[dn]\nCN=localhost\n[req]\ndistinguished_name = dn\n[EXT]\nsubjectAltName=DNS:localhost\nkeyUsage=digitalSignature\nextendedKeyUsage=serverAuth")
openssl x509 -in /etc/letsencrypt/live/interwebz.earth/fullchain.crt -out /etc/letsencrypt/live/interwebz.earth/fullchain.pem
    echo "test env"
else
    certbot --nginx -d interwebz.earth --non-interactive --agree-tos -m someodd@pm.me
    echo "not test env"
fi
cron

# ... the rest!
service nginx start
cabal run