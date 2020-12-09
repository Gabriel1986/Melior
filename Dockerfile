# Build temporary image
FROM mcr.microsoft.com/dotnet/sdk:3.1 AS build-env
WORKDIR /app
COPY . .
RUN apt-get update -y
RUN apt-get install nodejs -yq
RUN apt-get install npm -yq
RUN npm install npm@latest -g
RUN dotnet tool restore
RUN dotnet fake build target publish
WORKDIR /app

# Build runtime image
FROM mcr.microsoft.com/dotnet/sdk:3.1
RUN apt-get update -y
RUN apt-get install libgdiplus -yq
COPY --from=build-env /app/deploy /app
WORKDIR /app/Server
RUN cd /app/Server
ENTRYPOINT ["dotnet", "Server.dll"]