FROM node:8.12.0 as builder

RUN npm install -g create-elm-app@2.2.0 --unsafe-perm

WORKDIR /workdir

COPY . ./

RUN elm-app build


FROM nginx:1.15.5

COPY --from=builder /workdir/build /usr/share/nginx/html

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
