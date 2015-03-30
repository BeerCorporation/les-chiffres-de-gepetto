CREATE TABLE [dbo].[Words] (
    [Id]             INT           NOT NULL,
    [Word]           NVARCHAR (50) NOT NULL,
    [HasBeenChecked] BIT           NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);

