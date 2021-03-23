const mongoose = require("mongoose");
require("dotenv").config();
require("./../../config/mongo");

const visitCategoriesModel = require("./../../model/visitCategories")

async function create() {
  const toInsert = [
    {
      name: "Bar associatif",
      description: "toto",
      current: true
    },
    {
        name: "Doc associatives",
        description: "toto",
        current: true
    },
    {
        name: "Infos touristiques",
        description: "toto",
        current: true
    },
    {
        name: "Docs infos santé",
        description: "toto",
        current: true
    },
    {
        name: "activités Pôle Santé",
        description: "toto",
        current: true
    },
    {
        name: "Activités Pôle Culture",
        description: "toto",
        current: true
    },
    {
        name: "Autres activités",
        description: "toto",
        current: true
    },
    {
        name: "événements hors pôles",
        description: "toto",
        current: true
    },
    {
        name: "Activités des associations",
        description: "toto",
        current: true
    },
    {
        name: "Autres",
        description: "toto",
        current: true
    },

  ];
  return toInsert;
}

async function insertUsers() {
  try {
    await visitCategoriesModel.deleteMany();
    const inserted = await visitCategoriesModel.insertMany(await create());
    console.log(`seed users done : ${inserted.length} documents inserted !`);
    mongoose.connection.close().then((success) => console.log("WELL CLOSED"));
  } catch (err) {
    console.error(err);
    mongoose.connection.close().then((success) => console.log("WELL CLOSED"));
  }
}

insertUsers();
