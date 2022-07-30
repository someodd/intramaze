FROM interwebz_depends:latest

# In case source has changed (compared to depends).
WORKDIR /opt/example
COPY . /opt/example
RUN cabal update && cabal build

# NGINX configuration. Eventually the haskell server will be behind nginx as well.
RUN rm /etc/nginx/sites-enabled/default
COPY ./docker/nginx/*.conf /etc/nginx/conf.d
RUN chown -R www-data:www-data /opt/example/built

# Shouldn't expose both! Just expose 80! FIXME bu ti'll have to test out commands first
EXPOSE 80
EXPOSE 8080
#EXPOSE 3000
#EXPOSE 80/tcp
#EXPOSE 3000/tcp

# startup script
RUN mv /opt/example/docker/web-startup.sh /usr/local/bin/startup.sh
RUN chmod 777 /usr/local/bin/startup.sh
CMD /usr/local/bin/startup.sh
#ENTRYPOINT /bin/bash
# needs to also launch python server, or nginx