import React from 'react';
import { Toolbar } from '../Toolbar';
import { Dashboard } from '../Dashboard';

class Concorde extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            user: {},
            users: []
        };
    }

    componentDidMount() {
        this.setState({ 
            sessionId: localStorage.getItem('id'),
        });
    }

    render() {
        return (
            <div>
                <Toolbar></Toolbar>
                <Dashboard></Dashboard>
            </div>
        );
    }
}

export { Concorde };