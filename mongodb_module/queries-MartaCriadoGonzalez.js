////////////////////////// QUERIES BASICAS /////////////////////////////////////////
db.covid19ITAreg.updateMany(
  {},
  [{ "$set": { "convertedDate": { "$toDate": "$date" } }}]
);

db.covid19ITAreg.distinct('region',{region_code:{$lte: 10}})

db.covid19ITAreg.find({note:null}).count()
db.covid19ITAreg.find({note: {$ne: null}}, {_id:0, date:1, region:1, note:1})
db.covid19ITAreg.insert([
    {region_code:18, 
    region:'Calabria', 
    date:"2020-02-24T18:00:00", 
    note: 'This is an insert test'}
])
db.covid19ITAreg.deleteOne({note: {$regex: /insert/}})

//////////////////////////// PIPELINE DE AGREGACION //////////////////////////////

// query para la primera ola
var filter_primeraola = {
    convertedDate: 
    {$gte: ISODate("2020-02-01T00:00:00.0Z"),
    $lt: ISODate("2020-06-31T00:00:00.0Z")}
    
}

db.covid19ITAreg.aggregate([
    {$match: filter_primeraola},
    {$group: 
        {_id: "$convertedDate", positives_per_date: {$sum: '$new_positives'}}
    },
    {$group: 
        {_id: "", sum_positives_per_date: {$sum: '$positives_per_date'}}
    }
])

// query para la segunda ola
var filter_segundaola = {
    convertedDate: 
    {$gte: ISODate("2020-07-01T00:00:00.0Z"),
    $lt: ISODate("2020-10-28T00:00:00.0Z")}
}

db.covid19ITAreg.aggregate([
    {$match: filter_segundaola},
    {$group: 
        {_id: "$convertedDate", positives_per_date: {$sum: '$new_positives'}}
    },
    {$group: 
        {_id: "", sum_positives_per_date: {$sum: '$positives_per_date'}}
    }
])


// QUERIES CASOS POSITIVOS POR MES
db.covid19ITAreg.aggregate([
    {$group: 
        {_id: {mes: {$month: "$convertedDate"}}, 
        positives_per_month: {$sum: '$new_positives'}}
    }
])

//MUERTES SOBRE CASOS POSITIVOS DETECTADOS
db.covid19ITAreg.aggregate([
    {$group: 
        {_id: "$region", 
            total_deaths: {$sum: '$deaths'},
            sum_positives: {$sum: '$new_positives'}
        }
    },{$project: 
        {death_rate: {$divide: ["$total_deaths", '$sum_positives']}}
    }
    
]).sort({'death_rate': -1}).limit(5)

//CONGESTION DE UCIS
var filter_primeraola = {
    convertedDate: 
    {$gte: ISODate("2020-02-01T00:00:00.0Z"),
    $lt: ISODate("2020-06-31T00:00:00.0Z")}
    
}

var filter_segundaola = {
    convertedDate: 
    {$gte: ISODate("2020-07-01T00:00:00.0Z"),
    $lt: ISODate("2020-10-28T00:00:00.0Z")}
}

db.covid19ITAreg.aggregate([
    {$match: filter_segundaola},
    {$group:
        {_id: 
            {mes: {$month: "$convertedDate"}},
            max_hospitalized: {$max: "$total_hospitalized"},
            items:{$push: {total_ic:'$intensive_care', fecha: "$convertedDate"}} 
        }
    },{$unwind: '$items'}, 
    {$sort: {'max_hospitalized':-1, 'items.total_ic':-1}}, 
    {$project: 
        {ic_rate:{$divide: ["$items.total_ic", '$max_hospitalized']},
        max_hospitalized: '$max_hospitalized', intensive_care: '$items.total_ic',
        fecha: '$items.fecha'
        }
    }
])

// NUMERO DE CASOS POSITIVOS CONTRA TEST REALIZADOS
db.covid19ITAreg.aggregate([
    {$project: {month: {$month: '$convertedDate'}, tests: '$tests', total_cases: '$total_cases', region:'$region'}},
    {$match: {month: 10}},
    {$group: { 
        _id: "$region", 
        max_test_done: { $last: "$tests" },min_test_done: { $first: "$tests" },
        max_cases: {$last: '$total_cases'},min_cases: { $first: "$total_cases" }
    }},
    {$project: 
        {test_done:{$subtract: ["$max_test_done", '$min_test_done']},
        total_cases:{$subtract: ["$max_cases", '$min_cases']}}
    }
])


