module rec Server.Migrations

open System
open System.Reflection
open Npgsql
open SimpleMigrations
open SimpleMigrations.DatabaseProvider
open Shared.Library

let run (logger: Serilog.ILogger) (connectionString: string) : int64 * int64 =
    using (new NpgsqlConnection(connectionString)) (fun connection ->
        try
            let migrationsAssembly = Assembly.GetAssembly(typeof<CreateInitialTables>)
            let databaseProvider = PostgresqlDatabaseProvider (connection)
            let migrator = SimpleMigrator (migrationsAssembly, databaseProvider)

            logger.Information "Migrating DB"

            migrator.Load()
            let currentVersion = migrator.CurrentMigration.Version
            migrator.MigrateToLatest()
            logger.Information  "Finished migrating db"
            currentVersion, migrator.LatestMigration.Version

        with exn ->
            let exceptionType = exn.GetType().Name
            let exceptionMessage = exn.Message

            let errorMessage = sprintf "An '%s' exception with message '%s' happened during migration" exceptionType exceptionMessage
            logger.Error(exn, errorMessage)
            failwithf "%A" errorMessage
    )

[<Migration(1L, "Create initial tables")>]
type CreateInitialTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE Persons (
                    PersonId UUID PRIMARY KEY,
                    FirstName VARCHAR(255),
                    LastName VARCHAR(255),
                    LanguageCode VARCHAR(16),
                    Gender VARCHAR(16),
                    Title VARCHAR(32),
                    MainAddress JSON NOT NULL,
                    ContactAddress JSONB,
                    OtherAddresses JSONB,
                    MainTelephoneNumber VARCHAR(32),
                    MainTelephoneNumberComment VARCHAR(255),
                    MainEmailAddress VARCHAR(255),
                    MainEmailAddressComment VARCHAR(255),
                    OtherContactMethods JSONB
                );

                CREATE TABLE Organizations (
                    OrganizationId UUID PRIMARY KEY,
                    BuildingId UUID,
                    OrganizationNumber VARCHAR(12),
                    VatNumber VARCHAR(64),
                    VatNumberVerifiedOn DATE,
                    IsActive Boolean Default TRUE,
                    Name VARCHAR(255) NOT NULL,
                    Address JSONB NOT NULL,
                    MainTelephoneNumber VARCHAR(32),
                    MainTelephoneNumberComment VARCHAR(255),
                    MainEmailAddress VARCHAR(255),
                    MainEmailAddressComment VARCHAR(255),
                    OtherContactMethods JSONB
                );

                CREATE TABLE ProfessionalSyndics (
                    OrganizationId UUID PRIMARY KEY References Organizations(OrganizationId),
                    IsActive BOOLEAN DEFAULT TRUE
                );

                CREATE TABLE Buildings (
                    BuildingId UUID PRIMARY KEY,
                    IsActive BOOLEAN DEFAULT TRUE,
                    Code VARCHAR(16) NOT NULL,
                    Name VARCHAR(255) NOT NULL,
                    Address JSONB NOT NULL,
                    OrganizationNumber VARCHAR(12),
                    Remarks VARCHAR,
                    GeneralMeetingFrom DATE,
                    GeneralMeetingUntil DATE,
                    ConciergeOwnerId UUID,
                    ConciergePersonId UUID REFERENCES Persons(PersonId),
                    SyndicOwnerId UUID,
                    SyndicProfessionalSyndicId UUID REFERENCES ProfessionalSyndics(OrganizationId),
                    SyndicPersonId UUID REFERENCES Persons(PersonId),
                    YearOfConstruction INT,
                    YearOfDelivery INT
                );

                CREATE TABLE Owners (
                    PersonId UUID References Persons(PersonId),
                    BuildingId UUID References Buildings(BuildingId),
                    IsActive BOOLEAN DEFAULT TRUE,
                    IsResident BOOLEAN DEFAULT TRUE,
                    PRIMARY KEY (PersonId, BuildingId)
                );
                CREATE INDEX idx_Owners_PersonId ON Owners(PersonId);
                CREATE INDEX idx_Owners_BuildingId ON Owners(BuildingId);

                CREATE TABLE Lots (
                    LotId UUID PRIMARY KEY,
                    BuildingId UUID References Buildings(BuildingId),
                    Code VARCHAR(16) NOT NULL,
                    LotType VARCHAR(32) NOT NULL,
                    Description VARCHAR,
                    Floor INT,
                    Surface INT,
                    IsActive Boolean DEFAULT TRUE
                );
                CREATE INDEX idx_Lots_BuildingId ON Owners(BuildingId);

                CREATE TABLE LotOwners (
                    LotId UUID References Lots(LotId),
                    OrganizationId UUID References Organizations(OrganizationId), 
                    PersonId UUID References Persons(PersonId),
                    Role VARCHAR(32)
                );
                CREATE INDEX idx_LotOwners_LotId ON LotOwners(LotId);

                CREATE TABLE OrganizationTypes (
                    OrganizationTypeId UUID PRIMARY KEY,
                    Name VARCHAR(255) NOT NULL
                );

                CREATE TABLE OrganizationOrganizationTypeLinks (
                    OrganizationId UUID References Organizations(OrganizationId),
                    OrganizationTypeId UUID References OrganizationTypes(OrganizationTypeId),
                    PRIMARY KEY (OrganizationId, OrganizationTypeId)
                );
                CREATE INDEX idx_OrganizationOrganizationTypeLinks_OrganizationId ON OrganizationOrganizationTypeLinks(OrganizationId);

                CREATE TABLE ContactPersons (
                    OrganizationId UUID References Organizations(OrganizationId),
                    PersonId UUID References Persons(PersonId),
                    RoleWithinOrganization VARCHAR(32),
                    IsActive BOOLEAN DEFAULT TRUE,
                    PRIMARY KEY (OrganizationId, PersonId)
                );

                ALTER TABLE Organizations ADD CONSTRAINT FK_Organizations_Buildings
                    FOREIGN KEY (BuildingId)
                    REFERENCES Buildings(BuildingId);

                ALTER TABLE Buildings ADD CONSTRAINT fk_ConciergeOwnerId 
                    FOREIGN KEY (BuildingId, ConciergeOwnerId) 
                    REFERENCES Owners(BuildingId, PersonId);

                ALTER TABLE Buildings ADD CONSTRAINT fk_SyndicOwnerId
                    FOREIGN KEY (BuildingId, SyndicOwnerId)
                    REFERENCES Owners(BuildingId, PersonId);

                CREATE INDEX idx_ContactPersons_OrganizationId ON ContactPersons(OrganizationId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(2L, "Create mediafile tables")>]
type CreateMediaFileTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE MediaFiles (
                    Partition VARCHAR(64) NOT NULL,
                    EntityId UUID NOT NULL,
                    BuildingId UUID References Buildings(BuildingId),
                    FileId UUID PRIMARY KEY,
                    FileName VARCHAR(255),
                    FileSize INT,
                    MimeType VARCHAR(64),
                    UploadedOn TIMESTAMP
                );
                CREATE INDEX idx_MediaFiles_Partition_EntityId ON MediaFiles(Partition, EntityId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(3L, "Create user tables")>]
type CreateUserTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE Users (
                    UserId UUID PRIMARY KEY,
                    DisplayName VARCHAR(255),
                    EmailAddress VARCHAR(255) UNIQUE,
                    PreferredLanguageCode VARCHAR(16),
                    PasswordHash BYTEA,
                    UseTwoFac Boolean,
                    TwoFacSecret BYTEA,
                    IsActive Boolean DEFAULT TRUE
                );
                CREATE INDEX idx_Users_EmailAddress ON Users(EmailAddress);

                CREATE TABLE UserRoles (
                    UserId UUID,
                    Role VARCHAR(32),
                    BuildingId UUID REFERENCES Buildings(BuildingId),
                    OrganizationId UUID REFERENCES ProfessionalSyndics(OrganizationId)
                );
                CREATE INDEX idx_UserRoles_UserId ON UserRoles(UserId);

                CREATE TABLE RecoveryCodes (
                    UserId UUID,
                    RecoveryCodeHash BYTEA,
                    PRIMARY KEY (UserId, RecoveryCodeHash)
                );
                CREATE INDEX idx_RecoveryCodes_UserId ON RecoveryCodes(UserId);

                CREATE TABLE FailedTwoFacEvents (
                    UserId UUID,
                    TimeStamp TIMESTAMP,
                    PRIMARY KEY (UserId, TimeStamp)
                );
                CREATE INDEX idx_FailedTwoFacEvents_UserId ON FailedTwoFacEvents(UserId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(4L, "Create contract tables")>]
type CreateContractTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE Contracts (
                    ContractId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId),
                    ContractType VARCHAR(255) NOT NULL,
                    ContractFileId UUID REFERENCES MediaFiles(FileId),
                    ContractOrganizationId UUID REFERENCES Organizations(OrganizationId),
                    IsActive Boolean Default TRUE,
                    CreatedBy VARCHAR(255) NOT NULL,
                    CreatedAt TIMESTAMP NOT NULL,
                    LastUpdatedBy VARCHAR(255) NOT NULL,
                    LastUpdatedAt TIMESTAMP NOT NULL
                );
                CREATE INDEX idx_Contracts_BuildingId ON Contracts(BuildingId);

                CREATE TABLE Contracts_History (
                    ContractId UUID REFERENCES Contracts(ContractId),
                    BuildingId UUID REFERENCES Buildings(BuildingId),
                    ContractType VARCHAR(255) NOT NULL,
                    ContractFileId UUID REFERENCES MediaFiles(FileId),
                    ContractOrganizationId UUID REFERENCES Organizations(OrganizationId),
                    LastUpdatedBy VARCHAR(255) NOT NULL,
                    LastUpdatedAt TIMESTAMP NOT NULL,
                    PRIMARY KEY (ContractId, BuildingId, LastUpdatedAt)
                );

                CREATE TABLE ContractTypeAnswers (
                    BuildingId UUID REFERENCES Buildings(BuildingId) NOT NULL,
                    Question VARCHAR(64) NOT NULL,
                    IsTrue Boolean,
                    PRIMARY KEY (BuildingId, Question)
                );
                CREATE INDEX idx_ContractTypeAnswers_BuildingId ON ContractTypeAnswers(BuildingId);
            """
        )
    override u.Down () = failwith "Not supported"

//This is not correct.
[<Migration(5L, "Create many-to-many between Pro syndics and Buildings")>]
type CreateManyToManyProSyndicBuildings() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE ProfessionalSyndicBuildings (
                    OrganizationId UUID REFERENCES ProfessionalSyndics(OrganizationId),
                    BuildingId UUID REFERENCES Buildings(BuildingId),
                    PRIMARY KEY (OrganizationId, BuildingId)
                );
                CREATE INDEX idx_ProfessionalSyndicBuildings_OrganizationId ON ProfessionalSyndicBuildings(OrganizationId);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(6L, "Add DistributionKey tables")>]
type AddDistributionKeyTables() =
    inherit Migration()
    override u.Up () =
        u.Execute(
            """
                CREATE TABLE DistributionKeys (
                    DistributionKeyId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId),
                    Name VARCHAR(255),
                    DistributionType VARCHAR(32),
                    IsActive BOOLEAN DEFAULT TRUE,
                    CreatedBy VARCHAR(255) NOT NULL,
                    CreatedAt TIMESTAMP NOT NULL,
                    LastUpdatedBy VARCHAR(255) NOT NULL,
                    LastUpdatedAt TIMESTAMP NOT NULL
                );
                CREATE INDEX idx_DistributionKeys_BuildingId ON DistributionKeys(BuildingId);

                CREATE TABLE DistributionKeyLotsOrLotTypes (
                    DistributionKeyId UUID REFERENCES DistributionKeys(DistributionKeyId),
                    LotId UUID REFERENCES Lots(LotId),
                    LotType VARCHAR(32)
                );
                CREATE INDEX idx_DistributionKeyLotsOrLotTypes_DistributionKeyId ON DistributionKeyLotsOrLotTypes(DistributionKeyId);
                CREATE INDEX idx_DistributionKeyLotsOrLotTypes_LotId ON DistributionKeyLotsOrLotTypes(LotId);
                CREATE INDEX idx_DistributionKeyLotsOrLotTypes_LotType ON DistributionKeyLotsOrLotTypes(LotType);
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(7L, "Add BankAccount columns")>]
type AddBankAccountColumns() =
    inherit Migration()
    override u.Up () =
        //type BankAccount = { Description: string; IBAN: string; BIC: string }
        u.Execute(
            """
                ALTER TABLE Buildings
                    ADD COLUMN BankAccounts JSONB;

                ALTER TABLE Organizations
                    ADD COLUMN BankAccounts JSONB;

                ALTER TABLE Persons
                    ADD COLUMN BankAccounts JSONB;
            """
        )
    override u.Down () = failwith "Not supported"

[<Migration(8L, "Add 'share' column to the lot table")>]
type AddShareColumnToLotTable() =
    inherit Migration()
    override u.Up () = u.Execute("ALTER TABLE Lots ADD COLUMN Share INT")
    override u.Down () = failwith "Not supported"

[<Migration(9L, "Remove 'surface' column from the lot table")>]
type RemoveSurfaceFromLotTable() =
    inherit Migration()
    override u.Up () = u.Execute("ALTER TABLE Lots DROP COLUMN Surface")
    override u.Down () = failwith "Not supported"

[<Migration(10L, "Add Financial tables")>]
type AddFinancialTables() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

                ALTER TABLE DistributionKeys ADD COLUMN IF NOT EXISTS IncludeGroundFloor BOOLEAN DEFAULT TRUE;

                CREATE TABLE IF NOT EXISTS FinancialYears (
                    FinancialYearId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId) NOT NULL,
                    Code VARCHAR(32) NOT NULL,
                    StartDate TIMESTAMP NOT NULL,
                    EndDate TIMESTAMP NOT NULL,
                    IsClosed BOOLEAN DEFAULT FALSE,
                    IsDeleted BOOLEAN DEFAULT FALSE
                );

                CREATE TABLE IF NOT EXISTS FinancialCategories (
                    FinancialCategoryId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId) NOT NULL,
                    Code VARCHAR(32) NOT NULL,
                    Description VARCHAR(255) NOT NULL,
                    IsDeleted BOOLEAN DEFAULT FALSE
                );

                CREATE TABLE IF NOT EXISTS Invoices (
                    InvoiceId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId) NOT NULL,
                    FinancialYearId UUID REFERENCES FinancialYears(FinancialYearId) NOT NULL,
                    InvoiceNumber INT,
                    Description VARCHAR,
                    Cost DECIMAL,
                    VatRate INT,
                    FinancialCategoryId UUID REFERENCES FinancialCategories(FinancialCategoryId) NOT NULL,
                    DistributionKeyId UUID REFERENCES DistributionKeys(DistributionKeyId) NOT NULL,
                    OrganizationId UUID REFERENCES Organizations(OrganizationId) NOT NULL,
                    OrganizationBankAccount JSONB,
                    OrganizationInvoiceNumber VARCHAR(64),
                    BookingDate TIMESTAMP,
                    InvoiceDate TIMESTAMP,
                    DueDate TIMESTAMP,
                    CreatedBy VARCHAR(255) NOT NULL,
                    CreatedAt TIMESTAMP NOT NULL,
                    LastUpdatedBy VARCHAR(255) NOT NULL,
                    LastUpdatedAt TIMESTAMP NOT NULL,
                    IsDeleted BOOLEAN DEFAULT FALSE
                );
                CREATE Index IF NOT EXISTS idx_Invoices_BuildingId ON Invoices(BuildingId);
                CREATE UNIQUE INDEX IF NOT EXISTS uq_Invoices_FinancialYearId_InvoiceNumber ON Invoices(FinancialYearId, InvoiceNumber);

                CREATE TABLE IF NOT EXISTS Invoices_History (
                    InvoiceId UUID PRIMARY KEY,
                    BuildingId UUID REFERENCES Buildings(BuildingId) NOT NULL,
                    FinancialYearId UUID REFERENCES FinancialYears(FinancialYearId) NOT NULL,
                    InvoiceNumber INT NOT NULL,
                    Description VARCHAR,
                    Cost DECIMAL NOT NULL,
                    VatRate INT NOT NULL,
                    FinancialCategoryId UUID REFERENCES FinancialCategories(FinancialCategoryId) NOT NULL,
                    DistributionKeyId UUID REFERENCES DistributionKeys(DistributionKeyId) NOT NULL,
                    OrganizationId UUID REFERENCES Organizations(OrganizationId) NOT NULL,
                    OrganizationBankAccount JSONB NOT NULL,
                    OrganizationInvoiceNumber VARCHAR(64),
                    BookingDate TIMESTAMP NOT NULL,
                    InvoiceDate TIMESTAMP NOT NULL,
                    DueDate TIMESTAMP,
                    LastUpdatedBy VARCHAR(255) NOT NULL,
                    LastUpdatedAt TIMESTAMP NOT NULL
                );
            """
    override u.Down () = ()

[<Migration(11L, "Add StartDate and EndDate to LotOwners, remove IsActive")>]
type AddStartDateAndEndDateToLotOwners() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE LotOwners
                ADD COLUMN IF NOT EXISTS StartDate DATE,
                ADD COLUMN IF NOT EXISTS EndDate DATE,
                ADD COLUMN IF NOT EXISTS IsActive BOOLEAN;
            """

        u.Execute
            """
                Update LotOwners SET StartDate = '2020-10-01';
            """

        u.Execute
            """
                ALTER TABLE LotOwners ALTER COLUMN StartDate SET NOT NULL;
            """
    override u.Down () = ()

[<Migration(12L, "Add UsesVatNumber to Organizations")>]
type AddUsesVatNumberToOrganizations() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE Organizations ADD COLUMN IF NOT EXISTS UsesVatNumber BOOLEAN;
            """

        u.Execute
            """
                Update Organizations SET UsesVatNumber = (CASE WHEN BuildingId IS NULL THEN FALSE ELSE TRUE END);
            """

        u.Execute
            """
                ALTER TABLE Organizations ALTER COLUMN UsesVatNumber SET NOT NULL;
            """
    override u.Down () = ()

[<Migration(13L, "Add IsDeleted and LotOwnerId to LotOwners")>]
type AddIsDeletedAndLotOwnerIdToLotOwners() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE LotOwners ADD COLUMN IF NOT EXISTS IsDeleted BOOLEAN DEFAULT FALSE;
                ALTER TABLE LotOwners ADD COLUMN IF NOT EXISTS LotOwnerId UUID;
                ALTER TABLE LotOwners DROP COLUMN IF EXISTS IsActive;
            """

        u.Execute
            """
                UPDATE LotOwners SET IsDeleted = FALSE;
                UPDATE LotOwners SET LotOwnerId = uuid_generate_v4();
            """

        u.Execute 
            """
                ALTER TABLE LotOwners ALTER COLUMN LotOwnerId SET NOT NULL;
            """

        u.Execute
            """
                ALTER TABLE LotOwners DROP CONSTRAINT IF EXISTS lotowners_pkey;
                ALTER TABLE LotOwners ADD PRIMARY KEY  (LotOwnerId);
            """
    override u.Down () = ()

[<Migration(14L, "Add PhotoId and SharesTotal to Buildings")>]
type AddPhotoIdAndSharesTotalToBuildings() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE Buildings ADD COLUMN IF NOT EXISTS PictureId UUID;
                ALTER TABLE Buildings ADD COLUMN IF NOT EXISTS SharesTotal INT;
            """
    override u.Down () = ()

[<Migration(15L, "Remove Role from LotOwners and add LotOwnerContacts")>]
type RemoveRoleFromLotOwnersAndAddLotOwnerContacts() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE LotOwners DROP COLUMN IF EXISTS Role;

                CREATE TABLE IF NOT EXISTS LotOwnerContacts (
                    LotOwnerId UUID REFERENCES LotOwners(LotOwnerId) ON DELETE CASCADE NOT NULL,
                    BuildingId UUID,
                    ContactOwnerId UUID,
                    ContactPersonId UUID REFERENCES Persons(PersonId)
                );
                CREATE INDEX idx_LotOwnerContacts_LotOwnerId ON LotOwnerContacts(LotOwnerId);
                ALTER TABLE LotOwnerContacts ADD CONSTRAINT fk_ContactOwnerId
                    FOREIGN KEY (BuildingId, ContactOwnerId) 
                    REFERENCES Owners(BuildingId, PersonId);
            """
    override u.Down () = ()

[<Migration(16L, "Remove column ContractFileId from contracts")>]
type RemoveContractFileIdFromContacts() =
    inherit Migration ()
    override u.Up () =
        u.Execute
            """
                ALTER TABLE Contracts DROP COLUMN IF EXISTS ContractFileId;
                DROP TABLE IF EXISTS Contracts_History;
            """
    override u.Down () = ()

[<Migration(17L, "Add column IsDeleted to MediaFiles and Persons")>]
type AddColumnDeletedToMediaFilesAndPersons() =
    inherit Migration ()
    override u.Up () =
        u.Execute
            """
                ALTER TABLE MediaFiles ADD COLUMN IF NOT EXISTS Status VARCHAR(64) DEFAULT 'Temporary';
                UPDATE MediaFiles SET Status = 'Persisted';

                ALTER TABLE Persons ADD COLUMN IF NOT EXISTS IsDeleted BOOLEAN DEFAULT FALSE;
                UPDATE Persons SET IsDeleted = FALSE;
            """
    override u.Down () = ()

[<Migration(18L, "Convert predefined contracts to new format")>]
type ConvertPredefinedContractsToNewFormat() =
    inherit Migration ()
    override u.Up () = 
        let sql =
            [
                "ElevatorMaintenance"
                "ElevatorInspection"
                "CommonCentralHeatingInspection"
                "FireAlarmInspection"
                "FireExtinguisherInspection"
                "FireHoseReelInspection"
                "FireInsurance"
                "LiabilityInsurance"
                "CivilLiabilityForCoOwnerCouncil"
                "ElectricitySupplier"
                "WaterSupplier"
            ]
            |> List.map (fun predefined ->
                String.Format(
                    """
                        UPDATE Contracts SET ContractType = '["PredefinedContractType",{{"Type":"{0}"}}]'
                        WHERE ContractType like '%{0}%';
                    """
                    , (predefined)
                )
            )
            |> String.joinWith ""
        u.Execute sql
    override u.Down () = ()


[<Migration(19L, "Add a link between a financial category and a lot owner")>]
type AddLinkBetweenFinancialCategoryAndLotOwner() =
    inherit Migration ()
    override u.Up () = 
        u.Execute
            """
                ALTER TABLE FinancialCategories ADD COLUMN IF NOT EXISTS LotOwnerId UUID;
                ALTER TABLE FinancialCategories ADD CONSTRAINT fk_LotOwnerId
                    FOREIGN KEY (LotOwnerId)
                    REFERENCES LotOwners(LotOwnerId);
            """
    override u.Down () =
        ()