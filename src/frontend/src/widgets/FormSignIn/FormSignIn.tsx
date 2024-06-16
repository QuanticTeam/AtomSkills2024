import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Button, Checkbox, Form, Input, Space, Typography } from 'antd'
import { ReactNode, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { UserSignInDto, userSignInSchema } from '~/entities/User'
import { getIsDetailedApiError } from '~/shared/apiClient'
import { ROUTE_PATH_FORGOT_PASSWORD } from '~/shared/routing'

interface FormSignInProps {
  children: ReactNode
  onSubmit: SubmitHandler<UserSignInDto>
}

export function FormSignIn({ children, onSubmit }: FormSignInProps) {
  const { t } = useTranslation(FormSignIn.name)

  const {
    handleSubmit,
    control,
    formState: { errors },
  } = useForm<UserSignInDto>({
    values: {
      login: '',
      password: '',
      remember: true,
    },
    resolver: zodResolver(userSignInSchema),
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
            case 'SIGN_IN_WRONG_CREDENTIALS': {
              setSubmissionError(t('wrongCredentials'))
              return
            }
            default: {
              setSubmissionError(error.code)
            }
          }
        }
      }}
      className="w-4/5 m-auto"
      wrapperCol={{ span: 6, offset: 9 }}
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
              placeholder={t('login')}
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
              placeholder={t('password')}
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
                <Checkbox {...field}>{t('remember')}</Checkbox>
              </Form.Item>

              <Typography.Text>
                <Link to={ROUTE_PATH_FORGOT_PASSWORD}>{t('forgot')}</Link>
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
            {t('submit')}
          </Button>

          {!submissionError ? null : (
            <Typography.Text type="danger">{submissionError}</Typography.Text>
          )}

          {children}
        </Space>
      </Form.Item>
    </Form>
  )
}
