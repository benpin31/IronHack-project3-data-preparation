const mongoose = require("mongoose");
const { Schema, model } = mongoose;

const schema = new Schema( {
    name: {type: String, required: true},
    current: {type: Boolean, required: true}
})

const contactTypesModel = mongoose.model("contactTypes", schema);
module.exports = contactTypesModel;
