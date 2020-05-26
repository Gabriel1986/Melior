FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine
RUN mkdir /app
WORKDIR /app
COPY . .
WORKDIR /app/Server
RUN cd /app/Server
ENTRYPOINT ["dotnet", "Server.dll"][
