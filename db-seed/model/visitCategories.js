const mongoose = require("mongoose");
const { Schema, model } = mongoose;

const schema = new Schema( {
    name: {type: String, required: true},
    description: {type: String, required: true},
    current: {type: Boolean, required: true}
})

const visitCategoriesModel = mongoose.model("visitCategories", schema);
module.exports = visitCategoriesModel;
