import React from 'react';
import { connect } from 'react-redux'

import { Concorde } from '../Concorde';
import { LoginPage } from '../authentication';

function App(props) {
    if (props.loggedIn) {
        return (<Concorde></Concorde>);
    } else {
        return (<LoginPage></LoginPage>);
    }
}

function mapStateToProps(state) {
    return { loggedIn: state.login.loggedIn }
}

export default connect(mapStateToProps)(App);
