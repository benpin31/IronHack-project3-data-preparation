const mongoose = require("mongoose");
require("dotenv").config();
require("../../config/mongo");

const visitsModel = require("../../model/visits");
const visitCategoriesModel = require("../../model/visitCategories");
const contactTypesModel = require("../../model/contactTypes");

const seed = require('./seed.json')


getId = (propertyName, value, db) => {
    return  db.find(doc => doc[propertyName] === value)._id
}

const insert = async() => {

    try {
        await visitsModel.deleteMany();

        const visitCategories = await visitCategoriesModel.find() ;
        const contactTypes = await contactTypesModel.find() ;
    
        const toInsert = seed.map(visit => {
            return {
                visitCategory: getId("name", visit.category, visitCategories),
                contactType: getId("name", visit.contactType, contactTypes),
                date: visit.date
                }
            }
        )
        
        const inserted = await visitsModel.insertMany(toInsert);
        console.log(`seed users done : ${inserted.length} documents inserted !`);
        mongoose.connection.close().then((success) => console.log("WELL CLOSED"));

    } catch(err) {
        console.error(err);
        mongoose.connection.close().then((success) => console.log("WELL CLOSED"));
    }
}

insert()
