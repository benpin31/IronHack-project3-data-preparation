const mongoose = require("mongoose");
const { Schema, model } = mongoose;

const schema = new Schema( {
    visitCategory:       {
        type: Schema.Types.ObjectId,
        ref: "visitCategories",
        required: true
      },
    contactType: {
        type: Schema.Types.ObjectId, 
        ref: "contactTypes",
        required: true
    },
    date: {type: Date, default: Date.now()},
})

const visitsModel = mongoose.model("visits", schema);
module.exports = visitsModel;
