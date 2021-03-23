const mongoose = require("mongoose");
require("dotenv").config();
require("./../../config/mongo");

const visitCategoriesModel = require("./../../model/visits") ;
const visitCategoriesModel = require("./../../model/visitCategories") ;
const contactTypesModel = require("./../../model/contactTypes") ;

const seed = require('./seed.json')

console.log(seed)