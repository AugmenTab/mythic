FROM node:18-alpine

WORKDIR /foundryvtt

RUN apk update && apk add --no-cache bash

COPY ./foundryvtt /foundryvtt

EXPOSE 30000
