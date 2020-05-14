FROM quay.io/mojanalytics/rshiny:3.5.1

ENV AWS_DEFAULT_REGION eu-west-1
ENV PATH="/opt/shiny-server/bin:/opt/shiny-server/ext/node/bin:${PATH}"
ENV SHINY_APP=/srv/shiny-server
ENV NODE_ENV=production
ENV ALLOWED_PROTOCOLS="jsonp-polling"

WORKDIR /srv/shiny-server

# ENV SHINY_GAID <your google analytics token here>

# Add environment file individually so that next install command
# can be cached as an image layer separate from application code
ADD environment.yml environment.yml

# Conda packages install
RUN conda env update --file environment.yml -n base
RUN npm i -g ministryofjustice/analytics-platform-shiny-server#v0.0.3

## -----------------------------------------------------
## Uncomment if still using packrat alongside conda
## Install packrat itself then packages from packrat.lock
##ADD packrat packrat
##RUN R -e "install.packages('packrat', repos = 'https://cloud.r-project.org'); packrat::restore()"
## ------------------------------------------------------

# Add shiny app code
ADD . .

# Disable websockets on the server (to help Quantum grey-out issues)
RUN echo "disable_protocols websocket xdr-streaming xhr-streaming iframe-eventsource iframe-htmlfile xdr-polling xhr-polling iframe-xhr-polling;" >> /etc/shiny-server/shiny-server.conf

# Increase http timeout
RUN echo "http_keepalive_timeout 1800;" >> /etc/shiny-server/shiny-server.conf
RUN echo "app_idle_timeout 1800;" >> /etc/shiny-server/shiny-server.conf


USER shiny
CMD analytics-platform-shiny-server
EXPOSE 9999
