const mongoose = require("mongoose");
require("dotenv").config();
require("./../../config/mongo");

const contactTypesModel = require("./../../model/contactTypes")

async function create() {
  const toInsert = [
    {
      name: "Téléphone",
      current: true
    },
    {
        name: "Physique",
        current: true
    },

  ];
  return toInsert;
}

async function insertUsers() {
  try {
    await contactTypesModel.deleteMany();
    const inserted = await contactTypesModel.insertMany(await create());
    console.log(`seed users done : ${inserted.length} documents inserted !`);
    mongoose.connection.close().then((success) => console.log("WELL CLOSED"));
  } catch (err) {
    console.error(err);
    mongoose.connection.close().then((success) => console.log("WELL CLOSED"));
  }
}

insertUsers();
