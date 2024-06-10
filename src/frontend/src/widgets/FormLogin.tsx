import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Button, Checkbox, Form, Input, Space, Typography } from 'antd'
import { ReactNode, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { Link } from 'react-router-dom'
import { UserLoginDto, userLoginSchema } from '~/entities/User'
import { getIsDetailedApiError } from '~/shared/apiClient'

interface FormLoginProps {
  children: ReactNode
  onSubmit: SubmitHandler<UserLoginDto>
}

export function FormLogin({ children, onSubmit }: FormLoginProps) {
  const {
    handleSubmit,
    control,
    formState: { errors },
  } = useForm<UserLoginDto>({
    values: {
      login: '',
      password: '',
      remember: true,
    },
    resolver: zodResolver(userLoginSchema),
  })

  const [submissionError, setSubmissionError] = useState('')

  return (
    <Form
      onFinish={async (...args) => {
        try {
          await handleSubmit(onSubmit)(...args)
        } catch (error: unknown) {
          if (!getIsDetailedApiError(error)) throw error

          switch (error.code) {
            case 'LOGIN_WRONG_CREDENTIALS': {
              setSubmissionError('Неправильный логин или пароль') // TODO t
              return
            }
            default: {
              setSubmissionError(error.code)
            }
          }
        }
      }}
      className="w-96 mx-auto"
    >
      <Controller
        name="login"
        control={control}
        render={({ field }) => (
          <Form.Item
            help={errors.login?.message}
            validateStatus={errors.login?.message && 'error'}
          >
            <Input
              prefix={<UserOutlined className="text-gray-400" />}
              placeholder="Пользователь" // TODO t
              {...field}
            />
          </Form.Item>
        )}
      />
      <Controller
        name="password"
        control={control}
        render={({ field }) => (
          <Form.Item
            help={errors.password?.message}
            validateStatus={errors.password?.message && 'error'}
          >
            <Input.Password
              prefix={<LockOutlined className="text-gray-400" />}
              type="password"
              placeholder="Пароль" // TODO t
              {...field}
            />
          </Form.Item>
        )}
      />

      <Controller
        name="remember"
        control={control}
        render={({ field }) => (
          <Form.Item>
            <div className="flex justify-between items-center">
              <Form.Item
                name="remember"
                noStyle
                valuePropName="checked"
              >
                {/* TODO t */}
                <Checkbox {...field}>Запомнить меня</Checkbox>
              </Form.Item>

              <Typography.Text>
                {/* TODO t */}
                <Link to="/forgot">Не помню пароль</Link>
              </Typography.Text>
            </div>
          </Form.Item>
        )}
      />

      <Form.Item>
        <Space
          className="w-full"
          direction="vertical"
          classNames={{ item: 'flex justify-center' }}
        >
          <Button
            type="primary"
            htmlType="submit"
            className="w-full"
          >
            {/* TODO t */}
            Войти
          </Button>

          {!submissionError ? null : (
            // TODO t
            <Typography.Text type="danger">{submissionError}</Typography.Text>
          )}

          {children}
        </Space>
      </Form.Item>
    </Form>
  )
}
