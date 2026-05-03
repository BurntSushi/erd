pipeline {
    agent any

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Build') {
            steps {
                echo 'Building erd project...'
                sh 'cabal update'
                // Tambahkan --allow-newer untuk memaksa build pakai library baru
                sh 'cabal build --allow-newer'
            }
        }

        stage('Test') {
            steps {
                echo 'Running unit tests...'
                // Tambahkan juga di sini
                sh 'cabal test --allow-newer'
            }
        }
    }

    post {
        success {
            echo 'Pipeline completed successfully!'
        }
        failure {
            echo 'Pipeline failed!'
        }
    }
}
